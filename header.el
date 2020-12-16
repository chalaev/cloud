;;; cloud.el --- secure cloud storage and syncronization for text files  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Oleg Shalaev <oleg@chalaev.com>

;; Author:     Oleg Shalaev <oleg@chalaev.com>
;; Version:    the-version

;; Package-Requires: (cl epg dired-aux timezone diary-lib subr-x shalaev)
;; Keywords:   syncronization, cloud, gpg, encryption
;; URL:        https://github.com/chalaev/cloud

;;; Commentary:

;; This package shares text files between several computers used by one person.
;; For quick start and documentation see
;; https://github.com/chalaev/cloud
  
;;; Code:

;; (mapcar #'require '(cl epg dired-aux timezone diary-lib subr-x shalaev))
