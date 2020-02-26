;; This script defines the windows key + v hotkey, which will take the currently selected text (via Ctrl-C sent to the active window)
;; and paste it into autocad command line.  This is useful for debugging and experimenting with autocad.


;
#InstallMouseHook ;;testing something. -Neil
#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetKeyDelay 20
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.
DetectHiddenWindows, On
SetTitleMatchMode, 1
#singleInstance force

; Google the currently selected text.
; Sends Ctrl+c to the current window to capture the text
; preserves the original contents of the clipboard after it is done
#v::
	;Get the currently active window  
	WinGet, originalActiveWindow, ID, A

	;get the autocad window
	WinGet, acadMainWindowHandle, ID, AutoCAD ahk_exe acad.exe
	
	if (acadMainWindowHandle == "")
	{
		;; in this case, the autocad window was not found, so we will simply do nothing
	} else {
		;; in this case, the autocad window was found, so we will proceed to do stuff
		ClipSaved := ClipboardAll   ; Save the entire existing clipboard to a variable of your choice.
		sleep 100 ;I added this line as an empirical fix to a pathology I encountered using this hotkey in Adobe reader: Adobe reader complained that an error had occured copying to the clipboard.  Inserting this Sleep statement seemed to prevent this patholgy.  It is as if the above command to save the current clipboard ties up the clipboard file for a short period, and if you try to write to the clipboard in that period you get an error.  This Sleep waits long enough for that period to pass, or so it seems.
		; ... here make temporary use of the clipboard
		Send ^c  ; copy whatever text is selected, by sending ctrl-c to the active window
		Sleep 100 ;
		; Run, "http://google.com/search?q=%clipboard%"
		;;MsgBox about to send following to acad %Clipboard%
		
		WinActivate, ahk_id %acadMainWindowHandle%
		Sleep, 100
		
		;;cancel any running autocad command
		Send {Esc}
		Sleep, 60  
		Send {Esc}
		Sleep, 60 
		Send {Esc}
		Sleep, 60 
		
		acadCommand = % "(progn " . Clipboard . " )" . "`n" ;the terminal newline (nd the belensures that the command is actually executed.  ;; we wrap everything in (progn ) so that lisp comments and spaces, newlines, etc, in the code are ignored.
		;;SendRaw %acadCommand%
		
		; tempLispFile = % A_WorkingDir . "\" . "tempLisp.lsp"
		tempLispFile = % A_Temp . "\" . "tempLisp" . A_TickCount . ".lsp"
		IfExist, %tempLispFile%
				FileDelete, %tempLispFile%

		FileAppend, %acadCommand%, %tempLispFile%

		
		acadCommand = % "(load " . """" . addSlashes(tempLispFile) . """" . ")"

		Send {Esc}{Esc} ;;cancels any running autocad command
		SendRaw %acadCommand%
		Send {Enter}
		Sleep, 120
		
		;;Send {Enter} ;; send an extra enter just in case the text on the clipboard doesn't end with one, to execute the command in autocad.
		Sleep, 60
		
		Clipboard := ClipSaved   ; Restore the original clipboard. Note the use of Clipboard (not ClipboardAll).
		ClipSaved = ; set ClipSaved to empty to preserve memory, in case ClipboardAll was huge. (Autohtokey help recommmends doing this)
		
		;; restore the originally-active window
		WinActivate, ahk_id %originalActiveWindow%	
	}
return


addSlashes(x)
{
	return RegExReplace(x, "\\", "\\")
}