;; Handles wait lists, time estimates, and alternative equipment suggestions

;; Constants
(define-constant ERR-NOT-FOUND u404)
(define-constant ERR-ALREADY-QUEUED u400)
(define-constant ERR-UNAUTHORIZED u401)
(define-constant ADMIN tx-sender)

;; Equipment type and alternatives mapping
(define-map equipment-alternatives uint (list 3 uint))

;; Queue position for each user-equipment pair
(define-map queue-positions {user: principal, equipment-id: uint} uint)

;; Equipment queue metadata
(define-map equipment-queues uint {
  current-user: (optional principal),
  queue-length: uint,
  estimated-wait: uint,
  session-duration: uint
})

;; User currently using equipment
(define-map active-sessions uint {
  user: principal,
  start-block: uint
})

;; Initialize equipment with alternatives
(define-public (setup-equipment (equipment-id uint) (session-duration uint) (alternatives (list 3 uint)))
  (begin
    (asserts! (is-eq tx-sender ADMIN) (err ERR-UNAUTHORIZED))
    (map-set equipment-queues equipment-id {
      current-user: none,
      queue-length: u0,
      estimated-wait: u0,
      session-duration: session-duration
    })
    (map-set equipment-alternatives equipment-id alternatives)
    (ok true)))

;; Join queue for equipment
(define-public (join-queue (equipment-id uint))
  (let ((queue-data (unwrap! (map-get? equipment-queues equipment-id) (err ERR-NOT-FOUND)))
        (current-position (map-get? queue-positions {user: tx-sender, equipment-id: equipment-id})))
    (asserts! (is-none current-position) (err ERR-ALREADY-QUEUED))
    (let ((new-position (+ (get queue-length queue-data) u1))
          (estimated-wait (* (get session-duration queue-data) (get queue-length queue-data))))
      (map-set queue-positions {user: tx-sender, equipment-id: equipment-id} new-position)
      (map-set equipment-queues equipment-id (merge queue-data {
        queue-length: new-position,
        estimated-wait: estimated-wait
      }))
      (ok {position: new-position, estimated-wait: estimated-wait}))))

;; Leave queue
(define-public (leave-queue (equipment-id uint))
  (let ((position (unwrap! (map-get? queue-positions {user: tx-sender, equipment-id: equipment-id}) (err ERR-NOT-FOUND)))
        (queue-data (unwrap! (map-get? equipment-queues equipment-id) (err ERR-NOT-FOUND))))
    (map-delete queue-positions {user: tx-sender, equipment-id: equipment-id})
    (map-set equipment-queues equipment-id (merge queue-data {
      queue-length: (- (get queue-length queue-data) u1)
    }))
    (ok true)))

;; Start equipment session (first in queue)
(define-public (start-session (equipment-id uint))
  (let ((position (unwrap! (map-get? queue-positions {user: tx-sender, equipment-id: equipment-id}) (err ERR-NOT-FOUND)))
        (queue-data (unwrap! (map-get? equipment-queues equipment-id) (err ERR-NOT-FOUND))))
    (asserts! (is-eq position u1) (err ERR-UNAUTHORIZED))
    (asserts! (is-none (get current-user queue-data)) (err ERR-UNAUTHORIZED))
    (map-delete queue-positions {user: tx-sender, equipment-id: equipment-id})
    (map-set active-sessions equipment-id {
      user: tx-sender,
      start-block: stacks-block-height
    })
    (map-set equipment-queues equipment-id (merge queue-data {
      current-user: (some tx-sender),
      queue-length: (- (get queue-length queue-data) u1)
    }))
    (ok true)))

;; End equipment session
(define-public (end-session (equipment-id uint))
  (let ((session (unwrap! (map-get? active-sessions equipment-id) (err ERR-NOT-FOUND)))
        (queue-data (unwrap! (map-get? equipment-queues equipment-id) (err ERR-NOT-FOUND))))
    (asserts! (is-eq tx-sender (get user session)) (err ERR-UNAUTHORIZED))
    (map-delete active-sessions equipment-id)
    (map-set equipment-queues equipment-id (merge queue-data {
      current-user: none
    }))
    (ok true)))

;; Get queue status and alternatives
(define-read-only (get-queue-info (equipment-id uint))
  (let ((queue-data (map-get? equipment-queues equipment-id))
        (alternatives (map-get? equipment-alternatives equipment-id)))
    {
      queue: queue-data,
      alternatives: alternatives,
      user-position: (map-get? queue-positions {user: tx-sender, equipment-id: equipment-id})
    }))

;; Get available alternatives with shorter waits
(define-read-only (get-better-alternatives (equipment-id uint))
  (let ((current-wait (default-to u999 (get estimated-wait (map-get? equipment-queues equipment-id))))
        (alternatives (default-to (list) (map-get? equipment-alternatives equipment-id))))
    (filter is-better-option (map get-alt-info alternatives))))

(define-private (get-alt-info (alt-id uint))
  {
    equipment-id: alt-id,
    estimated-wait: (default-to u0 (get estimated-wait (map-get? equipment-queues alt-id))),
    queue-length: (default-to u0 (get queue-length (map-get? equipment-queues alt-id)))
  })

(define-private (is-better-option (alt-info {equipment-id: uint, estimated-wait: uint, queue-length: uint}))
  (< (get estimated-wait alt-info)
     (default-to u999 (get estimated-wait (map-get? equipment-queues u1)))))
