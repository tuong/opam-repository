(*start-pp-include
#include <unistd.h>
end-pp-include*)
include Mutex0

#if defined(_POSIX_TIMEOUTS) && (_POSIX_TIMEOUTS > 0)
(* POSIX thread functions *)
external mutex_timedlock : Mutex.t -> float -> bool = "unix_mutex_timedlock"

let timedlock mtx time = mutex_timedlock mtx (Time.to_float time)
#else
#warning "POSIX TMO not present; timedlock unavailable"
#endif
