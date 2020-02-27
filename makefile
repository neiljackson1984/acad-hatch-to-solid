getFullyQualifiedWindowsStylePath=$(shell cygpath --windows --absolute "$(1)")


default:
	autohotkey "$(call getFullyQualifiedWindowsStylePath,braids/as-built-tools/userInterface/executeInAutocad.ahk)" "$(call getFullyQualifiedWindowsStylePath,main.lsp) "