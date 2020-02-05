type t = {
  mutable finds : int;
  mutable cache_misses : int;
  mutable appended_hashes : int;
  mutable appended_offsets : int;
}

val reset_stats : unit -> unit

val get : unit -> t

val incr_finds : unit -> unit

val incr_cache_misses : unit -> unit

val incr_appended_hashes : unit -> unit

val incr_appended_offsets : unit -> unit

type cache_stats = { cache_misses : float }

type offset_stats = { offset_ratio : float; offset_significance : int }

val get_cache_stats : unit -> cache_stats

val get_offset_stats : unit -> offset_stats
