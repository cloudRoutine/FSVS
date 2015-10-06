namespace Plugin

open System
open System.Linq
open System.Threading
open System.Collections.Generic
open System.Windows
open System.Windows.Shapes
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Operations
open Microsoft.VisualStudio.Text.Classification
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Utilities
open System.Windows.Media

[<AutoOpen>]
module Data =
    let [<Literal>]  markerstring = "MarkerFormatDefinition/HighlightWordFormatDefinition"

type HighlightWordTag() =
    inherit TextMarkerTag(markerstring)

[<  Export(typeof<EditorFormatDefinition>)
;   Name(markerstring); UserVisible true
>]
type HighlightWordFormatDefinition() =
    inherit MarkerFormatDefinition()
    do
        base.BackgroundColor <- Nullable Colors.LightBlue
        base.ForegroundColor <- Nullable Colors.DarkBlue
        base.DisplayName <- "Highlight Word"
        base.ZOrder <- 5



type HighlightWordTagger
    ( view : ITextView
    , sourceBuffer:ITextBuffer
    , textSearchService:ITextSearchService
    , textStructureNavigator:ITextStructureNavigator  ) as self =
    let mutable view                   = view                  
    let mutable sourceBuffer           = sourceBuffer          
    let mutable textSearchService      = textSearchService     
    let mutable textStructureNavigator = textStructureNavigator
    let mutable wordSpans              = NormalizedSnapshotSpanCollection()
    let mutable currentWord            = Nullable () : SnapshotSpan Nullable 
    let mutable requestedPoint         = SnapshotPoint () 
    
    let updateLock = obj()

    let tagsChanged = Event<_,_>() 
    let tagsChangedStream = tagsChanged.Publish

    let wordExtentIsValid (currentRequest:SnapshotPoint) (word:TextExtent) =
        word.IsSignificant && currentRequest.Snapshot.GetText(word.Span.Span) |> Seq.exists(fun c -> Char.IsLetter c)
    
    do  
        self.View.Caret.PositionChanged.Add( self.CaretPositionChanged ) 
        self.View.LayoutChanged.Add( self.ViewLayoutChanged ) 

    member __.View                   with get() :ITextView = view       and set v = view                   <- v
    member __.SourceBuffer           with get()= sourceBuffer           and set v = sourceBuffer           <- v
    member __.TextSearchService      with get()= textSearchService      and set v = textSearchService      <- v
    member __.TextStructureNavigator with get()= textStructureNavigator and set v = textStructureNavigator <- v
    member __.WordSpans              with get()= wordSpans              and set v = wordSpans              <- v 
    member __.CurrentWord            with get()= currentWord            and set v = currentWord            <- v 
    member __.RequestedPoint         with get()= requestedPoint         and set v = requestedPoint         <- v 

    member __.SynchronousUpdate ( currentRequest : SnapshotPoint
                                , newSpans : NormalizedSnapshotSpanCollection
                                , newCurrentWord:SnapshotSpan Nullable  ) =
        lock updateLock (fun () ->
            if currentRequest <> self.RequestedPoint then () else
            self.WordSpans    <- newSpans
            self.CurrentWord  <- newCurrentWord
            tagsChanged.Trigger ( self
                                , SnapshotSpanEventArgs( SnapshotSpan( self.SourceBuffer.CurrentSnapshot
                                , 0
                                , self.SourceBuffer.CurrentSnapshot.Length)))
        ) 
        
    member __.UpdateWordAdornments () =
        let currentRequest = self.RequestedPoint
        let wordspans = List<SnapshotSpan>()
        let mutable word = self.TextStructureNavigator.GetExtentOfWord(currentRequest)
        let mutable foundword = true

        if not(wordExtentIsValid currentRequest word) then
            if  word.Span.Start <> currentRequest 
            ||  currentRequest = currentRequest.GetContainingLine().Start 
            ||  Char.IsWhiteSpace ((currentRequest-1).GetChar ()) then
                foundword <- false
            else
                word <- self.TextStructureNavigator.GetExtentOfWord (currentRequest-1)
                if not (wordExtentIsValid currentRequest word) then
                    foundword <- false
        if not foundword then
            self.SynchronousUpdate (currentRequest, NormalizedSnapshotSpanCollection (), Nullable())
        else    
            let currentWord = word.Span
            if self.CurrentWord.HasValue && currentWord = self.CurrentWord.Value then () 
            else
                let finddata = FindData ( currentWord.GetText ()
                                        , currentWord.Snapshot
                                        , FindOptions = (FindOptions.WholeWord ||| FindOptions.MatchCase))
                wordspans.AddRange (self.TextSearchService.FindAll finddata)
                if currentRequest = self.RequestedPoint then
                    self.SynchronousUpdate (currentRequest, NormalizedSnapshotSpanCollection (), Nullable currentWord)

    member __.UpdateCaretPosition (caretPosition:CaretPosition) =
        let point = caretPosition.Point.GetPoint(self.SourceBuffer, caretPosition.Affinity ) 

        if not point.HasValue then ()
        elif    self.CurrentWord.HasValue
            &&  self.CurrentWord.Value.Snapshot = self.View.TextSnapshot
            &&  point.Value.Difference (self.CurrentWord.Value.Start) >= 0
            &&  point.Value.Difference (self.CurrentWord.Value.End) <= 0 then ()
        else
            self.RequestedPoint <- point.Value

    member __.ViewLayoutChanged (e:TextViewLayoutChangedEventArgs) : unit =
        if e.NewSnapshot <> e.OldSnapshot then self.UpdateCaretPosition (self.View.Caret.Position)

    member __.CaretPositionChanged (e:CaretPositionChangedEventArgs) : unit =
        self.UpdateCaretPosition (e.NewPosition)

    member __.GetTags(spans:NormalizedSnapshotSpanCollection) = seq {
            if not self.CurrentWord.HasValue then () else
            let mutable currentWord = self.CurrentWord.Value
            let mutable wordSpans = self.WordSpans
            if spans.Count = 0 || wordSpans.Count = 0 then () else
            if spans.[0].Snapshot <> wordSpans.[0].Snapshot then
                wordSpans <- NormalizedSnapshotSpanCollection (wordSpans |> Seq.map( fun span -> 
                        span.TranslateTo (spans.[0].Snapshot,SpanTrackingMode.EdgeExclusive)))
                currentWord <- currentWord.TranslateTo (spans.[0].Snapshot, SpanTrackingMode.EdgeExclusive)
            if spans.OverlapsWith (NormalizedSnapshotSpanCollection (currentWord)) then
                yield TagSpan<HighlightWordTag> (currentWord, HighlightWordTag () ) :> ITagSpan<_>
            for span in NormalizedSnapshotSpanCollection.Overlap (spans, wordSpans ) do
                yield TagSpan<HighlightWordTag> (span, HighlightWordTag ()) :> ITagSpan<_>
        }

    interface ITagger<HighlightWordTag> with
        member __.GetTags(spans) = self.GetTags spans
        [<CLIEvent>] member __.TagsChanged = tagsChangedStream
        
        
[<Export(typeof<IViewTaggerProvider>)>]
[<ContentType "text">]
[<TagType (typeof<TextMarkerTag>)>]
type HighlightWordTaggerProvider [<ImportingConstructor>] 
    ( [<Import>] textSearchService : ITextSearchService
    , [<Import>] textStructureNavigatorSelector : ITextStructureNavigatorSelectorService) as self =
    let mutable textSearchService              = textSearchService 
    let mutable textStructureNavigatorSelector = textStructureNavigatorSelector 
    
    member __.TextSearchService              with get()= textSearchService              and set v = textSearchService <- v
    member __.TextStructureNavigatorSelector with get()= textStructureNavigatorSelector and set v = textStructureNavigatorSelector <- v

    member __.CreateTagger(textView: ITextView, buffer: ITextBuffer): ITagger<HighlightWordTag> = 
        if textView.TextBuffer <> buffer then null else
        let navigator = self.TextStructureNavigatorSelector.GetTextStructureNavigator buffer
        HighlightWordTagger(textView,buffer,self.TextSearchService,navigator) :> ITagger<_>          

    interface IViewTaggerProvider with
        member __.CreateTagger  (textView: ITextView, buffer: ITextBuffer): ITagger<_> = 
            self.CreateTagger(textView,buffer) :> obj :?> _
            
        

[<AutoOpen>]
module LexYacc =
    open System.Data


    [< Export; Name "fsyacc"; BaseDefinition "F#">]
    let fsyaccContentType = ContentTypeDefinition()

    [< Export; FileExtension ".fsy"; ContentType "fsyacc">]
    let fsyaccFileExtensionDefinition = FileExtensionToContentTypeDefinition()

    [< Export; Name "fslex"; BaseDefinition "F#">]
    let fslexContentType = ContentTypeDefinition()

    [< Export; FileExtension ".fsl"; ContentType "fslex">]
    let fslexFileExtensionDefinition = FileExtensionToContentTypeDefinition()


    let [<Literal>] glyphsize = 16.0
    let [<Literal>] yaccToken = "%token"

/// <summary>
/// 
/// </summary>
type YaccTag() = 
    inherit Tagging.TextMarkerTag(yaccToken)
    interface IGlyphTag

(*
    This is alll stuff it has 
    trouble with
    // especially
        if you put other things inside
*)

[<  Export(typeof<IGlyphFactoryProvider>)
;   Name "YaccGlyph"; ContentType "fsyacc"; TagType(typeof<YaccTag>); Order(After="VsTextMarker")
>]
type YaccTagger(classifier:IClassifier) =
    let tagsChanged = Event<_,_>()

    interface ITagger<YaccTag> with
        member __.GetTags spans =
          spans |> Seq.collect( fun span-> 
            classifier.GetClassificationSpans(span) 
            |> Seq.fold( fun acc classSpan ->
                if  classSpan.ClassificationType.Classification.ToLower().Contains("keyword") ||
                    classSpan.ClassificationType.Classification.ToLower().Contains("identifier") then
                    let index = classSpan.Span.GetText().IndexOf(yaccToken)
                    match index with
                    | -1 -> acc
                    | _  -> TagSpan<YaccTag>(SnapshotSpan(classSpan.Span.Start + index,yaccToken.Length), YaccTag()) :> ITagSpan<YaccTag> ::acc    
                else acc) [] :> seq<_>
            )

        [<CLIEvent>]
        member __.TagsChanged = tagsChanged.Publish


[<  Export(typeof<ITaggerProvider>)
;   ContentType "fsyacc"; TagType(typeof<YaccTag>)
>]
type YaccTagggerProvider [<ImportingConstructor>]
    ([<Import(typeof<IClassifierAggregatorService>)>] aggregatorService : IClassifierAggregatorService) =
    interface ITaggerProvider with
        member __.CreateTagger<'T when 'T :> ITag> (buffer:ITextBuffer) : ITagger<'T> =
            match isNull buffer  with
            | false -> null
            | true  -> downcast (YaccTagger(aggregatorService.GetClassifier(buffer)) |> box)


type YaccGlyphFactory () =
    interface IGlyphFactory with
        member __.GenerateGlyph(_,tag) =   
            if tag = null || not (tag :? YaccTag) then
                null
            else
                let eli = Ellipse()
                eli.Fill <- Brushes.AliceBlue
                eli.StrokeThickness <- 2.
                eli.Stroke <- Brushes.DarkBlue
                eli.Height <- glyphsize
                eli.Width <- glyphsize
                eli :> UIElement



[<  Export(typeof<IGlyphFactoryProvider>)
;   Name "YaccGlyph"; ContentType "fsyacc"; TagType(typeof<YaccTag>); Order(After="VsTextMarker")
>]
type YaccGlyphFactoryProvider () =
    interface IGlyphFactoryProvider with
        member __.GetGlyphFactory (_, _)  = 
            YaccGlyphFactory() :> IGlyphFactory              

(*
    HIGHLIGHTING SECTION
*)

//
//type [<AbstractClass>] classact () =
//    abstract swing : float -> int -> unit
//    abstract data : string with get, set
//    default val data = "Ⓦⓗⓐⓣ(☉൧ ಠ ？)" with get, set
//
//type MatterOfFact () =
//    inherit classact()
//    override __.swing f i = ()
//    override __.data with get() = "" and set v = ()
//
