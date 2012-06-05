(** Concurrency monad. *)

(** The THREAD monad. *)
module type THREAD =
sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val fail : exn -> 'a t
  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
  val finalize : (unit -> 'a t) -> (unit -> unit t) -> 'a t
  val sleep : float -> unit t
  val auto_yield : float -> unit -> unit t

  type mutex

  (** Create a recursive mutex that can be locked recursively by the same
    * thread; i.e., unlike a regular mutex,
    * [with_lock m (fun () -> ... with_lock m (fun () -> ... ))]
    * will not block. *)
  val create_recursive_mutex : unit -> mutex

  (* [with_lock m f] blocks until the [m] mutex can be locked, runs [f ()] and
   * unlocks the mutex (also if [f ()] raises an exception) *)
  val with_lock : mutex -> (unit -> 'a t) -> 'a t
end

(** Identity concurrency monad. Note that [Id.mutex] is a dummy type that
  * doesn't actually work like a mutex (i.e., [Id.with_lock m f] is equivalent
  * to [f ()]. This is so because n ocaml-sqlexpr's context [Sqlite] handles
  * can only be used from the thread where they were created, so there's no
  * need for mutual exclusion because trying to use the same handle from
  * different threads would be an error anyway. *)
module Id : THREAD with type 'a t = 'a

(** Lwt concurrency monad. *)
module Lwt : THREAD with type 'a t = 'a Lwt.t
