;;; hugo-export.el --- summary

;;; Commentary:
;;

;;; Code:

;;; 文字コーディング設定
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; package.el設定
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; 依存パッケージインストール
(package-install 'ox-hugo)

;; ユーザ設定
(setq user-full-name "Tsunenobu Kai")
(setq user-mail-address "kai2nenobu@gmail.com")

;; エクスポート処理
(require 'ox-hugo)
(with-temp-buffer
  (let ((default-directory (expand-file-name "content-org")))
    (insert-file-contents (expand-file-name "content.org"))
    (org-mode)
    (prin1 (org-hugo-export-wim-to-md :allsubtree nil))))

(provide 'hugo-export)
;;; hugo-export.el ends here
