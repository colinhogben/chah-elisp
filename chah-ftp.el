;;=======================================================================
;;	FTP sites of interest to Colin
;;=======================================================================

(defun chah-ftp (account &optional path)
  (find-file (if (fboundp 'tramp-file-name-handler)
		 (concat "/ftp:" account ":" path)
	       (concat "/" account ":" path))))

;;;###autoload
(defun amra ()
  "Open the AMRA root directory."
  (interactive)
  (chah-ftp "amra.org.uk@ftp.amra.org.uk" "/"))

;;;###autoload
(defun mythic ()
  "Open the Mythic Beasts PythonTech root directory."
  (interactive)
  (chah-ftp "pythontech@sphinx.mythic-beasts.com" "/home/pythontech/"))

;;;###autoload
(defun colinhogben ()
  "Open the colinhogben.com account home directory."
  (interactive)
  (chah-ftp "colinh@ftp.colinhogben.com"))

;;;###autoload
(defun netw-colinhogben ()
  "Open the colinhogben.com account on Netweaver portal"
  (interactive)
  (chah-ftp "colinhog@gilbert.netweaver.co.uk"))

;;;###autoload
(defun courtney ()
  "Open the Courtney Pianos account on Gradwell."
  (interactive)
  (chah-ftp "cpianos@ftp.gradwell.net" "/"))

;;;###autoload
(defun hmetaagility ()
  (interactive)
  (let ((ange-ftp-ftp-program-name "hftp"))
    (chah-ftp "xyz@metaagility.hftp")))

;;;###autoload
(defun pipeten ()
  "Open the Pipe Ten root directory"
  (interactive)
  (chah-ftp "pythontech@dweb4.pipeten.co.uk"))
