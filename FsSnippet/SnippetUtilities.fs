namespace FsSnippets

open System
open System.Collections.Generic
open System.Xml
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.OLE.Interop


// Code Snippets Schema Reference
// https://msdn.microsoft.com/en-us/library/vstudio/ms171418(v=vs.140).aspx
// https://msdn.microsoft.com/en-us/library/ms171418.aspx

// VSSDK Code Snippets

module SnippetUtilities =

    let [<Literal>] LanguageServiceGuidStr = "6B3BBD66-91B9-4039-9FCD-15FB46D25F9F"

    [< ProvideLanguageCodeExpansion
        (   LanguageServiceGuidStr
        ,   "TestSnippets"
        ,   0
        ,   "TestSnippets"
        ,   @"%InstallRoot%\TestSnippets\Snippets\%LCID%\TestSnippets.xml"
        ,   SearchPaths = @"%InstallRoot%\TestSnippets\Snippets\%LCID%\"
        ,   ForceCreateDirs = @"%InstallRoot%\TestSnippets\Snippets\%LCID%\")
    >]
    type internal TestCompletionCommandHandler () =
        interface IOleCommandTarget with
            member __.Exec( pguidCmdGroup, nCmdID, nCmdexecopt, pvaIn, pvaOut ) = 
                Unchecked.defaultof<_>

            member __.QueryStatus( pguidCmdGroup, cCmds, prgCmds, pCmdText ) = 
                Unchecked.defaultof<_>
           



