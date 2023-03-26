(in-package #:silver-brain.global)

(chameleon:defconfig
  (store/root-path (path:join (path:user-home) "silver-brain.dev/"))
  (store/attachments-path (op (path:join (store/root-path) "attachments/"))))

(chameleon:defprofile :dev)
