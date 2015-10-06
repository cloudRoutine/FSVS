namespace YaccTag

open System
open System.ComponentModel.Composition
open System.Windows
open System.Windows.Shapes
open System.Windows.Media
open EnvDTE
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Text.Classification
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Utilities

[<AutoOpen>]
module LexYacc =


    [<Export; Name("fsyacc"); BaseDefinition("F#")>]
    let fsyaccContentType = ContentTypeDefinition()

    [<Export; FileExtension(".fsy"); ContentType("fsyacc")>]
    let fsyaccFileExtensionDefinition = FileExtensionToContentTypeDefinition()

    [<Export; Name("fslex"); BaseDefinition("F#")>]
    let fslexContentType = ContentTypeDefinition()

    [<Export; FileExtension(".fsl"); ContentType("fslex")>]
    let fslexFileExtensionDefinition = FileExtensionToContentTypeDefinition()


    let [<Literal>] glyphsize = 16.0
    let [<Literal>] yaccToken = "%token"

/// <summary>
/// 
/// </summary>
type YaccTag() = 
    inherit Tagging.TextMarkerTag("token")
    interface IGlyphTag

(*

    This is alll stuff it has 
    trouble with

    // especially
        if you put other things inside


*)

[<  Export      (typeof<IGlyphFactoryProvider>)
;   Name        "YaccGlyph"
;   Order       (After="VsTextMarker")
;   ContentType "fsyacc"
;   TagType     (typeof<YaccTag>)
>]

// efef
//     fef
//       ef




type YaccTagger(classifier:IClassifier) =
    let tagsChanged = Event<_,_>()

    interface ITagger<YaccTag> with
        member __.GetTags spans =
         spans |> Seq.collect(fun span-> 
            classifier.GetClassificationSpans(span) 
            |> Seq.fold( fun acc classSpan ->
                if  classSpan.ClassificationType.Classification.ToLower().Contains("keyword") ||
                    classSpan.ClassificationType.Classification.ToLower().Contains("identifier") then
                    let index = classSpan.Span.GetText().IndexOf(yaccToken)
                    match index with
                    | -1 -> acc
                    | _  -> TagSpan<YaccTag>(SnapshotSpan(classSpan.Span.Start + index,yaccToken.Length), YaccTag()):> ITagSpan<YaccTag> ::acc    
                else acc) []:> seq<_>
            )

        [<CLIEvent>]
        member __.TagsChanged = tagsChanged.Publish


[<  Export      (typeof<ITaggerProvider>)
;   ContentType ("fsyacc")
;   TagType     (typeof<YaccTag>)
>]
type YaccTagggerProvider [<ImportingConstructor>]
    ([<Import(typeof<IClassifierAggregatorService>)>] aggregatorService : IClassifierAggregatorService) =
    interface ITaggerProvider with
        member __.CreateTagger<'T when 'T :> ITag> (buffer:ITextBuffer) : ITagger<'T> =
            match isNull buffer  with
            | false -> null
            | true -> downcast (YaccTagger(aggregatorService.GetClassifier(buffer)) |> box)


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


[<  Export      (typeof<IGlyphFactoryProvider>)
;   Name        ("YaccGlyph")
;   Order       (After="VsTextMarker")
;   ContentType ("fsyacc")
;   TagType     (typeof<YaccTag>)
>]
type YaccGlyphFactoryProvider () =
    interface IGlyphFactoryProvider with
        member __.GetGlyphFactory (_, _)  = 
            YaccGlyphFactory() :> IGlyphFactory              

            
            

        

                    


