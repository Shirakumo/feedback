(in-package #:org.shirakumo.feedback.client)

(defun run (program &rest args)
  (handler-case
      #-sbcl (uiop:run-program (list* program args) :output :string)
      #+sbcl (with-output-to-string (stream)
               (sb-ext:run-program program args :output stream #+win32 :window #+win32 :hide))
      (error ()
        "unknown (error running ~a)" program)))

(defun file (path)
  (with-open-file (stream path)
    (with-output-to-string (out)
      (loop with buffer = (make-array 4096 :element-type 'character)
            for read = (read-sequence buffer stream)
            while (< 0 read)
            do (write-string buffer out :end read)))))

(defun match (string &rest candidates)
  (loop for candidate in candidates
        thereis (search candidate string :test #'char-equal)))

(defun determine-os ()
  #+windows
  (values :windows (run "C:/Windows/System32/systeminfo.exe"))
  #+linux
  (values :linux (run "/usr/bin/env" "uname" "-a"))
  #+darwin
  (values :macos (run "sysctl" "kern.version"))
  #+freebsd
  (values :freebsd (run "/usr/bin/env" "uname" "-a"))
  #-(or windows linux darwin freebsd)
  (values :unknown "unknown"))

(defun determine-cpu ()
  (values #+x86-64 :amd64
          #+x86 :i686
          #+arm64 :arm64
          #+armv7l :armv7l
          #-(or x86-64 x86 arm64 armv7l) :unknown
          #+windows
          (run "C:/Windows/System32/Wbem/wmic.exe" "cpu" "list" "full")
          #+linux
          (file "/proc/cpuinfo")
          #+darwin
          (run "/bin/sh" "-c" "sysctl -a | grep machdep.cpu")
          #-(or windows linux darwin)
          "unknown"))

(defun determine-gpu ()
  (flet ((search-vendor (string)
           (cond ((match string "nvidia" "geforce") :nvidia)
                 ((match string "vmware") :vmware)
                 ((match string "virtualbox" "vbox" "virtual box") :virtualbox)
                 ((match string "intel") :intel)
                 ((or (search "AMD" string) (search "ATI" string) (match string "radeon")) :amd)
                 (T :unknown))))
    #+windows
    (let ((info (run "C:/Windows/System32/Wbem/wmic.exe" "PATH" "Win32_VideoController" "get" "/format:list")))
      (values (search-vendor info) info))
    #+linux
    (let ((info (run "/bin/sh" "-c" "lspci | grep ' VGA ' | cut -d' ' -f 1 | xargs -i lspci -v -s {}")))
      (values (search-vendor info) info))
    #+darwin
    (let ((info (run "/usr/sbin/system_profiler" "SPDisplaysDataType")))
      (values (search-vendor info) info))
    #-(or windows linux darwin)
    (values :unknown "unknown")))
