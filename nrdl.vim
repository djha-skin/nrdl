" Vim syntax file
" Language: NRDL
" Maintainer: Daniel Jay Haskin <me@djha.skin>
" Last Change: 2023-04-23
" Version: 0.1.0

if !exists("main_syntax")
    " Quit if already loaded
    if exists("b:current_syntax")
        finish
    endif
    let main_syntax = "nrdl"
endif
" Case sensitive
syntax case match

setlocal iskeyword=+'

syntax region nrdlComment start=/#/ end=/$/

syntax keyword nrdlBoolean true 'true' false 'false'
syntax keyword nrdlNull null 'null'
syntax match nrdlBarewordSymbol /[^-.0-9\]\[{}>|][^[:blank]]*/
syntax region nrdlQuotedSymbol start=/'/ skip=/\\'/ end=/'/
syntax cluster nrdlSymbol contains=nrdlQuotedSymnbol,nrdlBarewordSymbol

syntax match nrdlNumber /\(0\|[1-9-.][0-9-.eE]*\)/

syntax region nrdlQuotedString start=/"/ skip=/\\"/ end=/"/

syntax match nrdlEndMultiline /^[:,[:blank:]]*\^/
syntax region nrdlVerbatim start=/[:,[:blank:]]*|/ end=/$/ nextgroup=nrdlVerbatim,nrdlEndMultiline 
syntax region nrdlProse start=/[:,[:blank:]]*>/ end=/$/ nextgroup=nrdlVerbatim,nrdlEndMultiline
syntax cluster nrdlString contains=nrdlQuotedString,ndrlVerbatim,nrdlProse

syntax region nrdlObject matchgroup=nrdlBraces start=/{/ end=/}/ contains=ALL
syntax region nrdlArray matchgroup=nrdlBrackets start=/\[/ end=/\]/ contains=ALL
syntax cluster nrdlDelimiter contains=nrdlBraces,nrdlBrackets

hi def link nrdlString          String
hi def link nrdlSymbol          Label
hi def link nrdlDelimiter       Delimiter
hi def link jsonEscape          Special
hi def link nrdlNumber          Number
hi def link nrdlNull            Function
hi def link nrdlBoolean         Boolean

let b:current_syntax = "json"
if main_syntax == 'json'
  unlet main_syntax
endif
