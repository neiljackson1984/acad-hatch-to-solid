; sends keystrokes to the autocad window that will close and reopen the current file

#singleInstance FORCE
#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
;; SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.


SetTitleMatchMode 2


;Get the currently active window  
WinGet, originalActiveWindow, ID, A

WinGet, acadMainWindowHandle, ID, AutoCAD ahk_exe acad.exe

if (acadMainWindowHandle == "")
{
  MsgBox, "Acad needs to be running for this script to work. Please start Acad and try again."
	 ExitApp
}


WinActivate, ahk_id %acadMainWindowHandle%
Sleep, 60
Send !f   ;;opens the file menu
Sleep, 30
Send c    ;;executes close


; Wait until save-changes confirmation dialog appears or continue if it does not appear within two seconds.
WinWait, AutoCAD ahk_exe acad.exe, Save changes to, 2
if errorlevel
{
   ;; in this case, the confirmation dialog never appeared so we don't need to do anytning
} else {
	;;in this case, the confirmation dialog did appear so now we need to
	;; tell it "no, don't save changes" (by sending "n" keystroke)
	WinGet, ConfirmationDialogHandle, ID,  ;;get the last found window (i.e. the confirmation dialog that was found by WinWait
	Sleep, 60
	ControlSend, , n, ahk_id %ConfirmationDialogHandle%
}

WinActivate, ahk_id %acadMainWindowHandle%
Sleep, 60
Send !f   ;;opens the file menu
Sleep, 30
Send 1    ;;opens the most recently opened file
Sleep, 4500 ;;wait for the autocad file to load


;ControlSend, , asdfafasdfasdf, ahk_id %acadMainWindowHandle%

WinActivate, ahk_id %originalActiveWindow%

addSlashes(x)
{
	return RegExReplace(x, "\\", "\\")
}

