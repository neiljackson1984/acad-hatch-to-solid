; 2015/12/28
; This script allows me to use the AppsKey (The key between right ctrl 
;  and right alt that brings up a context menu) as if
; it were the middle mouse button.  This was useful when using 
;  the trackpad on a the Toshiba laptop, which has no middle mouse button.
; I needed the middle mouse button to be able to pan in Autocad.
; -Neil
; ===============================================

#singleInstance force
#InstallKeybdHook

;; ====attempt 1:
; ~LButton & RButton::MouseClick, Middle
; ~RButton & LButton::MouseClick, Middle

;; ====attempt 2:
;; AppsKey::MouseClick, Middle, , , , , D
;; AppsKey UP::MouseClick, Middle, , , , , U

;; ====attempt 3:  This had the desired effect.
AppsKey::MButton