;;;; winlock.lisp
(in-package #:jaganet-client)

(cffi:define-foreign-library winlock
  (:windows "WinLockDll.dll"))

(cffi:use-foreign-library winlock)

(cffi:defcfun "Desktop_Show_Hide" :int (bShowHide :boolean))
(cffi:defcfun "Taskbar_Show_Hide" :int (bShowHide :boolean))
(cffi:defcfun "TaskSwitching_Enable_Disable" :int (bShowHide :boolean))

(cffi:defcfun "Process_Desktop" :int (szDesktopName :string) (szPath :string))
