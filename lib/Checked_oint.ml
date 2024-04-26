open Ctypes

(* This is a workaround to force dynamic linking, see
   <https://github.com/ocaml/dune/issues/10461#issuecomment-2082149852>. *)
external _force_link : unit -> unit = "checked_oint_force_link"

exception Out_of_range

module C = struct
  open Foreign

  type u128

  let u128 : u128 structure typ = structure "u128"

  let u128_high = field u128 "high" uint64_t

  let u128_low = field u128 "low" uint64_t

  let () = seal u128

  type i128

  let i128 : i128 structure typ = structure "i128"

  let i128_high = field i128 "high" uint64_t

  let i128_low = field i128 "low" uint64_t

  let () = seal i128

  let print_u32, print_u64, print_u128, print_i128 =
      let summon (suffix, ty) =
          foreign ("checked_oint_print_" ^ suffix) (ty @-> ptr char @-> returning int)
      in
      ( summon ("u32", int32_t)
      , summon ("u64", int64_t)
      , summon ("u128", u128)
      , summon ("i128", i128) )
  ;;

  let scan_u32, scan_u64, scan_i32, scan_i64, scan_u128, scan_i128 =
      let summon (suffix, ty) =
          foreign
            ("checked_oint_scan_" ^ suffix)
            (string @-> ptr ty @-> int @-> returning int)
      in
      ( summon ("u32", int32_t)
      , summon ("u64", int64_t)
      , summon ("i32", int32_t)
      , summon ("i64", int64_t)
      , summon ("u128", u128)
      , summon ("i128", i128) )
  ;;

  let scan_int =
      foreign "checked_oint_scan_int" (string @-> ptr int @-> int @-> returning int)
  ;;

  (* Avoid problems with the sign bit by calling into C. *)
  let ( (bit_not_i8, bit_not_i16)
      , (shift_left_i8, shift_right_i8)
      , (shift_left_i16, shift_right_i16) )
    =
      let summon_op1 (name, suffix, ty) =
          foreign ("checked_oint_" ^ name ^ "_" ^ suffix) (ty @-> returning ty)
      in
      let summon_op2 (name, suffix, ty) =
          foreign ("checked_oint_" ^ name ^ "_" ^ suffix) (ty @-> ty @-> returning ty)
      in
      let summon_shifts (suffix, ty) =
          summon_op2 ("shift_left", suffix, ty), summon_op2 ("shift_right", suffix, ty)
      in
      ( (summon_op1 ("bit_not", "i8", int), summon_op1 ("bit_not", "i16", int))
      , summon_shifts ("i8", int)
      , summon_shifts ("i16", int) )
  ;;

  let ( ( equal_u128
        , compare_u128
        , u128_min
        , u128_max
        , u128_of_int_unchecked
        , add_u128
        , sub_u128
        , mul_u128
        , div_u128
        , rem_u128
        , bit_not_u128
        , bit_or_u128
        , bit_and_u128
        , bit_xor_u128
        , shift_left_u128
        , shift_right_u128 )
      , ( equal_i128
        , compare_i128
        , i128_min
        , i128_max
        , i128_of_int_unchecked
        , add_i128
        , sub_i128
        , mul_i128
        , div_i128
        , rem_i128
        , bit_not_i128
        , bit_or_i128
        , bit_and_i128
        , bit_xor_i128
        , shift_left_i128
        , shift_right_i128 ) )
    =
      let summon_op1 (suffix, name, ty) =
          foreign ("checked_oint_" ^ name ^ "_" ^ suffix) (ty @-> returning ty)
      in
      let summon_op2 (suffix, name, ty) =
          foreign ("checked_oint_" ^ name ^ "_" ^ suffix) (ty @-> ty @-> returning ty)
      in
      let summon (suffix, ty) =
          ( foreign ("checked_oint_equal_" ^ suffix) (ty @-> ty @-> returning bool)
          , foreign ("checked_oint_compare_" ^ suffix) (ty @-> ty @-> returning int)
          , foreign ("checked_oint_min_" ^ suffix) (void @-> returning ty)
          , foreign ("checked_oint_max_" ^ suffix) (void @-> returning ty)
          , foreign ("checked_oint_of_int_unchecked_" ^ suffix) (int @-> returning ty)
          , summon_op2 (suffix, "add", ty)
          , summon_op2 (suffix, "sub", ty)
          , summon_op2 (suffix, "mul", ty)
          , summon_op2 (suffix, "div", ty)
          , summon_op2 (suffix, "rem", ty)
          , summon_op1 (suffix, "bit_not", ty)
          , summon_op2 (suffix, "bit_or", ty)
          , summon_op2 (suffix, "bit_and", ty)
          , summon_op2 (suffix, "bit_xor", ty)
          , summon_op2 (suffix, "shift_left", ty)
          , summon_op2 (suffix, "shift_right", ty) )
      in
      summon ("u128", u128), summon ("i128", i128)
  ;;
end

(* Polymorphic comparison operators will raise an exception on [unit -> unit]. *)
type 'a wrapper = (unit -> unit) * 'a

let wrap x = (((fun () -> ()), x) [@coverage off])

let unwrap (_f, x) = x

let wrap_op1 f x = wrap (f (unwrap x))

let wrap_op2 f x y = wrap (f (unwrap x) (unwrap y))

let wrap_to_string f x =
    let min_i128_s = "-170141183460469231731687303715884105728" in
    let max_int_print_size = String.length min_i128_s + (* the null character *) 1 in
    assert (max_int_print_size = 41);
    let buffer = allocate_n char ~count:max_int_print_size in
    string_from_ptr ~length:(f (unwrap x) buffer) buffer
;;

let equal_int_wrapper x y = unwrap x = unwrap y

let pp_int_wrapper fmt x = Format.pp_print_string fmt (string_of_int (unwrap x))

let compare_int_wrapper x y = compare (unwrap x) (unwrap y)

type int_wrapper = int wrapper

type u8 = int_wrapper [@@deriving eq, show, ord]

type u16 = int_wrapper [@@deriving eq, show, ord]

let u32_to_string = wrap_to_string C.print_u32

let equal_u32_wrapper x y = Int32.equal (unwrap x) (unwrap y)

let pp_u32_wrapper fmt x = Format.pp_print_string fmt (u32_to_string x)

let compare_u32_wrapper x y = Int32.unsigned_compare (unwrap x) (unwrap y)

type u32_wrapper = Int32.t wrapper

type u32 = u32_wrapper [@@deriving eq, show, ord]

let u64_to_string = wrap_to_string C.print_u64

let equal_u64_wrapper x y = Int64.equal (unwrap x) (unwrap y)

let pp_u64_wrapper fmt x = Format.pp_print_string fmt (u64_to_string x)

let compare_u64_wrapper x y = Int64.unsigned_compare (unwrap x) (unwrap y)

type u64_wrapper = Int64.t wrapper

type u64 = u64_wrapper [@@deriving eq, show, ord]

let u128_to_string = wrap_to_string C.print_u128

let equal_u128_wrapper x y = C.equal_u128 (unwrap x) (unwrap y)

let pp_u128_wrapper fmt x = Format.pp_print_string fmt (u128_to_string x)

let compare_u128_wrapper x y = C.compare_u128 (unwrap x) (unwrap y)

type u128_wrapper = C.u128 structure wrapper

type u128 = u128_wrapper [@@deriving eq, show, ord]

type i8 = int_wrapper [@@deriving eq, show, ord]

type i16 = int_wrapper [@@deriving eq, show, ord]

let equal_i32_wrapper x y = Int32.equal (unwrap x) (unwrap y)

let pp_i32_wrapper fmt x = Format.pp_print_string fmt (Int32.to_string (unwrap x))

let compare_i32_wrapper x y = Int32.compare (unwrap x) (unwrap y)

type i32_wrapper = Int32.t wrapper

type i32 = i32_wrapper [@@deriving eq, show, ord]

let equal_i64_wrapper x y = Int64.equal (unwrap x) (unwrap y)

let pp_i64_wrapper fmt x = Format.pp_print_string fmt (Int64.to_string (unwrap x))

let compare_i64_wrapper x y = Int64.compare (unwrap x) (unwrap y)

type i64_wrapper = Int64.t wrapper

type i64 = i64_wrapper [@@deriving eq, show, ord]

let i128_to_string = wrap_to_string C.print_i128

let equal_i128_wrapper x y = C.equal_i128 (unwrap x) (unwrap y)

let pp_i128_wrapper fmt x = Format.pp_print_string fmt (i128_to_string x)

let compare_i128_wrapper x y = C.compare_i128 (unwrap x) (unwrap y)

type i128_wrapper = C.i128 structure wrapper

type i128 = i128_wrapper [@@deriving eq, show, ord]

let extract_i64_field x field = wrap (Unsigned.UInt64.to_int64 (getf (unwrap x) field))

[@@@coverage off]

type generic =
  | U8 of u8
  | U16 of u16
  | U32 of u32
  | U64 of u64
  | U128 of u128
  | I8 of i8
  | I16 of i16
  | I32 of i32
  | I64 of i64
  | I128 of i128
[@@deriving eq, show]

[@@@coverage on]

module type Basic = sig
  type t [@@deriving eq, show, ord]

  val bits : int
  val min_int : t
  val max_int : t
  val of_int_unchecked : int -> t
  val add_unchecked : t -> t -> t
  val sub_unchecked : t -> t -> t
  val mul_unchecked : t -> t -> t
  val div_unchecked : t -> t -> t
  val rem_unchecked : t -> t -> t
  val shift_left_unchecked : t -> t -> t
  val shift_right_unchecked : t -> t -> t
  val bit_not : t -> t
  val bit_or : t -> t -> t
  val bit_and : t -> t -> t
  val bit_xor : t -> t -> t
  val of_int_exn : int -> t
  val of_string_exn : string -> t
  val to_generic : t -> generic
end
[@@ocamlformat "module-item-spacing = compact"]

let determine_base s =
    let cut_prefix prefix = String.(sub s (length prefix) (length s - length prefix)) in
    let ( let$ ) (prefix, base) k =
        if String.starts_with ~prefix s
        then base, cut_prefix prefix
        else if String.starts_with ~prefix:("-" ^ prefix) s
        then base, "-" ^ cut_prefix ("-" ^ prefix)
        else k ()
    in
    let$ () = "0b", 2 in
    let$ () = "0B", 2 in
    let$ () = "0o", 8 in
    let$ () = "0O", 8 in
    let$ () = "0x", 16 in
    let$ () = "0X", 16 in
    10, s
;;

module Inherit_int_basic (S : sig
    val min_int : int_wrapper
    val max_int : int_wrapper
  end) =
struct
  let of_int_unchecked x = wrap x
  let add_unchecked = wrap_op2 ( + )
  let sub_unchecked = wrap_op2 ( - )
  let mul_unchecked = wrap_op2 ( * )
  let div_unchecked = wrap_op2 ( / )
  let rem_unchecked = wrap_op2 Int.rem
  let bit_or = wrap_op2 ( lor )
  let bit_and = wrap_op2 ( land )
  let bit_xor = wrap_op2 ( lxor )

  let of_int_exn x =
      if x < unwrap S.min_int || x > unwrap S.max_int
      then raise Out_of_range
      else of_int_unchecked x
  ;;

  let of_string_exn s =
      let base, s = determine_base s in
      let x_ptr = allocate int 0 in
      let rc = C.scan_int s x_ptr base in
      let n = !@x_ptr in
      if rc != 0 || n < unwrap S.min_int || n > unwrap S.max_int
      then raise Out_of_range
      else wrap n
  ;;
end
[@@ocamlformat "module-item-spacing = compact"]

module U8_basic : Basic with type t = u8 = struct
  type t = u8 [@@deriving eq, show, ord]

  let bits = 8
  let min_int = wrap 0
  let max_int = wrap 255
  let shift_left_unchecked = wrap_op2 (fun x y -> (x lsl y) land unwrap max_int)
  let shift_right_unchecked = wrap_op2 (fun x y -> x lsr y)
  let bit_not = wrap_op1 (fun x -> lnot x land unwrap max_int)
  let to_generic x = U8 x [@@coverage off]

  include Inherit_int_basic (struct
      let min_int = min_int
      let max_int = max_int
    end)
end
[@@ocamlformat "module-item-spacing = compact"]

module U16_basic : Basic with type t = u16 = struct
  type t = u16 [@@deriving eq, show, ord]

  let bits = 16
  let min_int = wrap 0
  let max_int = wrap 65535
  let shift_left_unchecked = wrap_op2 (fun x y -> (x lsl y) land unwrap max_int)
  let shift_right_unchecked = wrap_op2 (fun x y -> x lsr y)
  let bit_not = wrap_op1 (fun x -> lnot x land unwrap max_int)
  let to_generic x = U16 x [@@coverage off]

  include Inherit_int_basic (struct
      let min_int = min_int
      let max_int = max_int
    end)
end
[@@ocamlformat "module-item-spacing = compact"]

module U32_basic : Basic with type t = u32 = struct
  type t = u32 [@@deriving eq, show, ord]

  let bits = 32
  let min_int = wrap Int32.zero
  let max_int = wrap (Int32.lognot (unwrap min_int))
  let of_int_unchecked x = wrap (Int32.of_int x)
  let add_unchecked = wrap_op2 Int32.add
  let sub_unchecked = wrap_op2 Int32.sub
  let mul_unchecked = wrap_op2 Int32.mul
  let div_unchecked = wrap_op2 Int32.unsigned_div
  let rem_unchecked = wrap_op2 Int32.unsigned_rem
  let shift_left_unchecked = wrap_op2 (fun x y -> Int32.(shift_left x (to_int y)))

  let shift_right_unchecked =
      wrap_op2 (fun x y -> Int32.(shift_right_logical x (to_int y)))
  ;;

  let bit_not = wrap_op1 Int32.lognot
  let bit_or = wrap_op2 Int32.logor
  let bit_and = wrap_op2 Int32.logand
  let bit_xor = wrap_op2 Int32.logxor

  let of_int_exn x =
      if x < 0 || Int64.(of_int x > pred (shift_left 1L 32))
      then raise Out_of_range
      else of_int_unchecked x
  ;;

  let of_string_exn s =
      let base, s = determine_base s in
      let x_ptr = allocate int32_t Int32.zero in
      let rc = C.scan_u32 s x_ptr base in
      if rc != 0 then raise Out_of_range else wrap !@x_ptr
  ;;

  let to_generic x = U32 x [@@coverage off]
end
[@@ocamlformat "module-item-spacing = compact"]

module U64_basic : Basic with type t = u64 = struct
  type t = u64 [@@deriving eq, show, ord]

  let bits = 64
  let min_int = wrap Int64.zero
  let max_int = wrap (Int64.lognot (unwrap min_int))
  let of_int_unchecked x = wrap (Int64.of_int x)
  let add_unchecked = wrap_op2 Int64.add
  let sub_unchecked = wrap_op2 Int64.sub
  let mul_unchecked = wrap_op2 Int64.mul
  let div_unchecked = wrap_op2 Int64.unsigned_div
  let rem_unchecked = wrap_op2 Int64.unsigned_rem
  let shift_left_unchecked = wrap_op2 (fun x y -> Int64.(shift_left x (to_int y)))

  let shift_right_unchecked =
      wrap_op2 (fun x y -> Int64.(shift_right_logical x (to_int y)))
  ;;

  let bit_not = wrap_op1 Int64.lognot
  let bit_or = wrap_op2 Int64.logor
  let bit_and = wrap_op2 Int64.logand
  let bit_xor = wrap_op2 Int64.logxor
  let of_int_exn x = if x < 0 then raise Out_of_range else of_int_unchecked x

  let of_string_exn s =
      let base, s = determine_base s in
      let x_ptr = allocate int64_t Int64.zero in
      let rc = C.scan_u64 s x_ptr base in
      if rc != 0 then raise Out_of_range else wrap !@x_ptr
  ;;

  let to_generic x = U64 x [@@coverage off]
end
[@@ocamlformat "module-item-spacing = compact"]

module U128_basic : Basic with type t = u128 = struct
  type t = u128 [@@deriving eq, show, ord]

  let bits = 128
  let min_int = wrap (C.u128_min ())
  let max_int = wrap (C.u128_max ())
  let of_int_unchecked x = wrap (C.u128_of_int_unchecked x)
  let add_unchecked = wrap_op2 C.add_u128
  let sub_unchecked = wrap_op2 C.sub_u128
  let mul_unchecked = wrap_op2 C.mul_u128
  let div_unchecked = wrap_op2 C.div_u128
  let rem_unchecked = wrap_op2 C.rem_u128
  let shift_left_unchecked = wrap_op2 C.shift_left_u128
  let shift_right_unchecked = wrap_op2 C.shift_right_u128
  let bit_not = wrap_op1 C.bit_not_u128
  let bit_or = wrap_op2 C.bit_or_u128
  let bit_and = wrap_op2 C.bit_and_u128
  let bit_xor = wrap_op2 C.bit_xor_u128
  let of_int_exn x = if x < 0 then raise Out_of_range else of_int_unchecked x

  let of_string_exn s =
      let base, s = determine_base s in
      let x_ptr = addr (make C.u128) in
      let rc = C.scan_u128 s x_ptr base in
      if rc != 0 then raise Out_of_range else wrap !@x_ptr
  ;;

  let to_generic x = U128 x [@@coverage off]
end
[@@ocamlformat "module-item-spacing = compact"]

module I8_basic : Basic with type t = i8 = struct
  type t = i8 [@@deriving eq, show, ord]

  let bits = 8
  let min_int = wrap (-128)
  let max_int = wrap 127
  let shift_left_unchecked = wrap_op2 C.shift_left_i8
  let shift_right_unchecked = wrap_op2 C.shift_right_i8
  let bit_not = wrap_op1 C.bit_not_i8
  let to_generic x = I8 x [@@coverage off]

  include Inherit_int_basic (struct
      let min_int = min_int
      let max_int = max_int
    end)
end
[@@ocamlformat "module-item-spacing = compact"]

module I16_basic : Basic with type t = i16 = struct
  type t = i16 [@@deriving eq, show, ord]

  let bits = 16
  let min_int = wrap (-32768)
  let max_int = wrap 32767
  let shift_left_unchecked = wrap_op2 C.shift_left_i16
  let shift_right_unchecked = wrap_op2 C.shift_right_i16
  let bit_not = wrap_op1 C.bit_not_i16
  let to_generic x = I16 x [@@coverage off]

  include Inherit_int_basic (struct
      let min_int = min_int
      let max_int = max_int
    end)
end
[@@ocamlformat "module-item-spacing = compact"]

module I32_basic : Basic with type t = i32 = struct
  type t = i32 [@@deriving eq, show, ord]

  let bits = 32
  let min_int = wrap Int32.min_int
  let max_int = wrap Int32.max_int
  let of_int_unchecked x = wrap (Int32.of_int x)
  let add_unchecked = wrap_op2 Int32.add
  let sub_unchecked = wrap_op2 Int32.sub
  let mul_unchecked = wrap_op2 Int32.mul
  let div_unchecked = wrap_op2 Int32.div
  let rem_unchecked = wrap_op2 Int32.rem
  let shift_left_unchecked = wrap_op2 (fun x y -> Int32.(shift_left x (to_int y)))
  let shift_right_unchecked = wrap_op2 (fun x y -> Int32.(shift_right x (to_int y)))
  let bit_not = wrap_op1 Int32.lognot
  let bit_or = wrap_op2 Int32.logor
  let bit_and = wrap_op2 Int32.logand
  let bit_xor = wrap_op2 Int32.logxor

  let of_int_exn x =
      let min_int_as_int64, max_int_as_int64 =
          Int64.of_int32 (unwrap min_int), Int64.of_int32 (unwrap max_int)
      in
      if Int64.of_int x < min_int_as_int64 || Int64.of_int x > max_int_as_int64
      then raise Out_of_range
      else of_int_unchecked x
  ;;

  let of_string_exn s =
      let base, s = determine_base s in
      let x_ptr = allocate int32_t Int32.zero in
      let rc = C.scan_i32 s x_ptr base in
      if rc != 0 then raise Out_of_range else wrap !@x_ptr
  ;;

  let to_generic x = I32 x [@@coverage off]
end
[@@ocamlformat "module-item-spacing = compact"]

module I64_basic : Basic with type t = i64 = struct
  type t = i64 [@@deriving eq, show, ord]

  let bits = 64
  let min_int = wrap Int64.min_int
  let max_int = wrap Int64.max_int
  let of_int_unchecked x = wrap (Int64.of_int x)
  let add_unchecked = wrap_op2 Int64.add
  let sub_unchecked = wrap_op2 Int64.sub
  let mul_unchecked = wrap_op2 Int64.mul
  let div_unchecked = wrap_op2 Int64.div
  let rem_unchecked = wrap_op2 Int64.rem
  let shift_left_unchecked = wrap_op2 (fun x y -> Int64.(shift_left x (to_int y)))
  let shift_right_unchecked = wrap_op2 (fun x y -> Int64.(shift_right x (to_int y)))
  let bit_not = wrap_op1 Int64.lognot
  let bit_or = wrap_op2 Int64.logor
  let bit_and = wrap_op2 Int64.logand
  let bit_xor = wrap_op2 Int64.logxor
  let of_int_exn x = of_int_unchecked x

  let of_string_exn s =
      let base, s = determine_base s in
      let x_ptr = allocate int64_t Int64.zero in
      let rc = C.scan_i64 s x_ptr base in
      if rc != 0 then raise Out_of_range else wrap !@x_ptr
  ;;

  let to_generic x = I64 x [@@coverage off]
end
[@@ocamlformat "module-item-spacing = compact"]

module I128_basic : Basic with type t = i128 = struct
  type t = i128 [@@deriving eq, show, ord]

  let bits = 128
  let min_int = wrap (C.i128_min ())
  let max_int = wrap (C.i128_max ())
  let of_int_unchecked x = wrap (C.i128_of_int_unchecked x)
  let add_unchecked = wrap_op2 C.add_i128
  let sub_unchecked = wrap_op2 C.sub_i128
  let mul_unchecked = wrap_op2 C.mul_i128
  let div_unchecked = wrap_op2 C.div_i128
  let rem_unchecked = wrap_op2 C.rem_i128
  let shift_left_unchecked = wrap_op2 C.shift_left_i128
  let shift_right_unchecked = wrap_op2 C.shift_right_i128
  let bit_not = wrap_op1 C.bit_not_i128
  let bit_or = wrap_op2 C.bit_or_i128
  let bit_and = wrap_op2 C.bit_and_i128
  let bit_xor = wrap_op2 C.bit_xor_i128
  let of_int_exn x = of_int_unchecked x

  let of_string_exn s =
      let base, s = determine_base s in
      let x_ptr = addr (make C.i128) in
      let rc = C.scan_i128 s x_ptr base in
      if rc != 0 then raise Out_of_range else wrap !@x_ptr
  ;;

  let to_generic x = I128 x [@@coverage off]
end
[@@ocamlformat "module-item-spacing = compact"]

module type S = sig
  type t [@@deriving eq, show, ord]

  val bits : int
  val zero : t
  val one : t
  val all_ones : t
  val min_int : t
  val max_int : t
  val is_signed : bool
  val of_int_exn : int -> t
  val of_string_exn : string -> t
  val succ : t -> t option
  val pred : t -> t option
  val neg : t -> t option
  val abs : t -> t option
  val add : t -> t -> t option
  val sub : t -> t -> t option
  val mul : t -> t -> t option
  val div : t -> t -> t option
  val rem : t -> t -> t option
  val bit_not : t -> t
  val bit_or : t -> t -> t
  val bit_and : t -> t -> t
  val bit_xor : t -> t -> t
  val shift_left : t -> t -> t option
  val shift_right : t -> t -> t option
  val min : t -> t -> t
  val max : t -> t -> t
  val succ_exn : t -> t
  val pred_exn : t -> t
  val neg_exn : t -> t
  val abs_exn : t -> t
  val add_exn : t -> t -> t
  val sub_exn : t -> t -> t
  val mul_exn : t -> t -> t
  val div_exn : t -> t -> t
  val rem_exn : t -> t -> t
  val shift_left_exn : t -> t -> t
  val shift_right_exn : t -> t -> t
  val to_string : t -> string
  val to_generic : t -> generic
end
[@@ocamlformat "module-item-spacing = compact"]

let overflow, underflow, div_by_zero = None, None, None

module Make (S : Basic) : S with type t = S.t = struct
  include S
  open S

  let zero, one = of_int_unchecked 0, of_int_unchecked 1

  let all_ones = if equal min_int zero then max_int else of_int_unchecked (-1)

  let is_signed = not (equal min_int zero)

  let pred x = if equal x min_int then underflow else Some (sub_unchecked x one)

  let succ x = if equal x max_int then overflow else Some (add_unchecked x one)

  let neg x =
      if equal x min_int && is_signed
      then overflow
      else if compare x zero != 0 && not is_signed
      then underflow
      else Some (sub_unchecked zero x)
  ;;

  let abs x =
      if equal x min_int && is_signed
      then overflow
      else Some (if compare x zero < 0 then sub_unchecked zero x else x)
  ;;

  let add x y =
      if compare y zero >= 0 && compare x (sub_unchecked max_int y) > 0
      then overflow
      else if compare y zero < 0 && compare x (sub_unchecked min_int y) < 0
      then underflow
      else Some (add_unchecked x y)
  ;;

  let sub x y =
      if compare y zero < 0 && compare x (add_unchecked max_int y) > 0
      then overflow
      else if compare y zero >= 0 && compare x (add_unchecked min_int y) < 0
      then underflow
      else Some (sub_unchecked x y)
  ;;

  let mul x y =
      match () with
      | _
        when (compare x zero > 0
              && compare y zero > 0
              && compare x (div_unchecked max_int y) > 0)
             || (compare x zero < 0
                 && compare y zero < 0
                 && compare x (div_unchecked max_int y) < 0) -> overflow
      | _
        when (compare x zero < 0
              && compare y zero > 0
              && compare x (div_unchecked min_int y) < 0)
             || (compare x zero > 0
                 && is_signed
                 && compare y (of_int_unchecked (-1)) < 0
                 && compare x (div_unchecked min_int y) > 0) -> underflow
      | _ -> Some (mul_unchecked x y)
  ;;

  let div x y =
      if equal y zero
      then div_by_zero
      else if equal x min_int && is_signed && equal y (of_int_unchecked (-1))
      then overflow
      else Some (div_unchecked x y)
  ;;

  let rem x y =
      if equal y zero
      then div_by_zero
      else if equal x min_int && is_signed && equal y (of_int_unchecked (-1))
      then overflow
      else Some (rem_unchecked x y)
  ;;

  let shift_left x y =
      if compare y zero < 0
      then underflow
      else if compare y (of_int_unchecked bits) >= 0
      then overflow
      else Some (shift_left_unchecked x y)
  ;;

  let shift_right x y =
      if compare y zero < 0
      then underflow
      else if compare y (of_int_unchecked bits) >= 0
      then overflow
      else Some (shift_right_unchecked x y)
  ;;

  let min x y = if compare x y <= 0 then x else y

  let max x y = if compare x y >= 0 then x else y

  let succ_exn, pred_exn, neg_exn, abs_exn =
      let check f x =
          match f x with
          | Some z -> z
          | None -> raise Out_of_range
      in
      check succ, check pred, check neg, check abs
  ;;

  let add_exn, sub_exn, mul_exn, div_exn, rem_exn, shift_left_exn, shift_right_exn =
      let check f x y =
          match f x y with
          | Some z -> z
          | None -> raise Out_of_range
      in
      ( check add
      , check sub
      , check mul
      , check div
      , check rem
      , check shift_left
      , check shift_right )
  ;;

  let to_string = show
end

module U8 : S with type t = u8 = Make (U8_basic)

module U16 : S with type t = u16 = Make (U16_basic)

module U32 : S with type t = u32 = Make (U32_basic)

module U64 : S with type t = u64 = Make (U64_basic)

module U128 : sig
  include S with type t = u128

  val split : t -> u64 * u64
end = struct
  include Make (U128_basic)

  let split (x : u128) : u64 * u64 =
      C.(extract_i64_field x u128_high, extract_i64_field x u128_low)
  ;;
end

module I8 : S with type t = i8 = Make (I8_basic)

module I16 : S with type t = i16 = Make (I16_basic)

module I32 : S with type t = i32 = Make (I32_basic)

module I64 : S with type t = i64 = Make (I64_basic)

module I128 : sig
  include S with type t = i128

  val split : t -> u64 * u64
end = struct
  include Make (I128_basic)

  let split (x : i128) : u64 * u64 =
      C.(extract_i64_field x i128_high, extract_i64_field x i128_low)
  ;;
end

[@@@coverage off]

type signedness =
  | Unsigned
  | Signed
[@@deriving eq, show, enumerate]

type bitness =
  | Bits8
  | Bits16
  | Bits32
  | Bits64
  | Bits128
[@@deriving eq, show, enumerate]

type int_ty = signedness * bitness [@@deriving eq, show, enumerate]

[@@@coverage on]

module type Singleton = sig
  type t

  include S with type t := t

  val value : t
end

module type Pair = sig
  type t

  include S with type t := t

  val value : t * t
end

let ops : int_ty -> (module S) = function
  | Unsigned, Bits8 -> (module U8)
  | Unsigned, Bits16 -> (module U16)
  | Unsigned, Bits32 -> (module U32)
  | Unsigned, Bits64 -> (module U64)
  | Unsigned, Bits128 -> (module U128)
  | Signed, Bits8 -> (module I8)
  | Signed, Bits16 -> (module I16)
  | Signed, Bits32 -> (module I32)
  | Signed, Bits64 -> (module I64)
  | Signed, Bits128 -> (module I128)
;;

let singleton =
    let make (type a) (module S : S with type t = a) x =
        (module struct
          include S

          let value = x
        end : Singleton)
    in
    function
    | U8 x -> make (module U8) x
    | U16 x -> make (module U16) x
    | U32 x -> make (module U32) x
    | U64 x -> make (module U64) x
    | U128 x -> make (module U128) x
    | I8 x -> make (module I8) x
    | I16 x -> make (module I16) x
    | I32 x -> make (module I32) x
    | I64 x -> make (module I64) x
    | I128 x -> make (module I128) x
[@@coverage off]
;;

let pair_exn =
    let make (type a) (module S : S with type t = a) (x, y) =
        (module struct
          include S

          let value = x, y
        end : Pair)
    in
    function
    | U8 x, U8 y -> make (module U8) (x, y)
    | U16 x, U16 y -> make (module U16) (x, y)
    | U32 x, U32 y -> make (module U32) (x, y)
    | U64 x, U64 y -> make (module U64) (x, y)
    | U128 x, U128 y -> make (module U128) (x, y)
    | I8 x, I8 y -> make (module I8) (x, y)
    | I16 x, I16 y -> make (module I16) (x, y)
    | I32 x, I32 y -> make (module I32) (x, y)
    | I64 x, I64 y -> make (module I64) (x, y)
    | I128 x, I128 y -> make (module I128) (x, y)
    | (U8 _ | U16 _ | U32 _ | U64 _ | U128 _ | I8 _ | I16 _ | I32 _ | I64 _ | I128 _), _
      -> invalid_arg "Checked_oint.pair_exn"
[@@coverage off]
;;

let generic_int_ty = function
  | U8 _ -> Unsigned, Bits8
  | U16 _ -> Unsigned, Bits16
  | U32 _ -> Unsigned, Bits32
  | U64 _ -> Unsigned, Bits64
  | U128 _ -> Unsigned, Bits128
  | I8 _ -> Signed, Bits8
  | I16 _ -> Signed, Bits16
  | I32 _ -> Signed, Bits32
  | I64 _ -> Signed, Bits64
  | I128 _ -> Signed, Bits128
[@@coverage off]
;;
