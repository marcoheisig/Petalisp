;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-native-backend)

(defclass translation-unit ()
  ((%n-values :initarg :n-values :reader n-values)
   (%reduction-spec :initarg :reduction-spec :reader reduction-spec)
   (%form-builders :initarg :form-builders :reader form-builders)
   (%expression-table :initarg :cse-table :reader cse-table)
   (%variable-table :initarg :cse-table :reader cse-table)))


