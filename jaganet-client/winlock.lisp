(in-package #:jaganet-client)

(cffi:define-foreign-library winlock
  (:windows "WinLockDll.dll"))

(cffi:use-foreign-library winlock)

(cffi:defctype winlock-code :int)

(cffi:defcfun "Taskbar_Show_Hide" winlock-code (bShowHide :boolean))
(cffi:defcfun "TaskSwitching_Enable_Disable" winlock-code (bShowHide :boolean))

(cffi:defcfun "Process_Desktop" winlock-code (szDesktopName :string) (szPath :string))
