open Checked_oint

let check_bool msg (actual, expected) = Alcotest.(check' bool) ~msg ~actual ~expected

let check_string msg (actual, expected) = Alcotest.(check' string) ~msg ~actual ~expected

let int_modules_list = List.map ops all_of_int_ty

let limits =
    [ "0", "255"
    ; "-128", "127"
    ; "0", "65535"
    ; "-32768", "32767"
    ; "0", "4294967295"
    ; "-2147483648", "2147483647"
    ; "0", "18446744073709551615"
    ; "-9223372036854775808", "9223372036854775807"
    ; "0", "340282366920938463463374607431768211455"
    ; ( "-170141183460469231731687303715884105728"
      , "170141183460469231731687303715884105727" )
    ]
;;

let cases = []

let check_ocaml_oints () =
    List.iter
      (fun (module S : S) ->
         let check msg x =
             check_string msg (S.(to_string (of_int_exn x)), string_of_int x)
         in
         let check_raises msg x =
             Alcotest.check_raises msg Out_of_range (fun () -> ignore (S.of_int_exn x))
         in
         check "Of zero" 0;
         check "Of 100" 100;
         if S.is_signed
         then check "Of -100" (-100)
         else check_raises "Unsigned underflow" (-100);
         match S.bits with
         | 8 when S.is_signed ->
           check_raises "Signed underflow (8-bit)" (-129);
           check_raises "Signed overflow (8-bit)" 128
         | 8 -> check_raises "Unsigned overflow (8-bit)" 256
         | 16 when S.is_signed ->
           check_raises "Signed underflow (16-bit)" (-32769);
           check_raises "Signed overflow (16-bit)" 32768
         | 16 -> check_raises "Unsigned overflow (16-bit)" 65536
         | 32 when S.is_signed ->
           check_raises "Signed underflow (32-bit)" (-2147483649);
           check_raises "Signed overflow (32-bit)" 2147483648
         | 32 -> check_raises "Unsigned overflow (32-bit)" 4294967296
         | _ -> ())
      int_modules_list
;;

let cases = ("Check OCaml integers", check_ocaml_oints) :: cases

let perform_ops () =
    int_modules_list
    |> List.iter (fun (module S : S) ->
      let check msg (actual, expected) =
          Alcotest.(check' (module S)) ~msg ~actual ~expected
      in
      let check_raises msg (comp : S.t Lazy.t) =
          Alcotest.check_raises msg Out_of_range (fun () -> ignore (Lazy.force comp))
      in
      let check_succ_pred () =
          check "Successor" S.(succ_exn (of_int_exn 10), of_int_exn 11);
          check_raises "Successor (overflow)" (lazy S.(succ_exn max_int));
          check "Predecessor" S.(pred_exn (of_int_exn 10), of_int_exn 9);
          check_raises "Predecessor (underflow)" (lazy S.(pred_exn min_int))
      in
      let check_neg () =
          check "Negation of zero" S.(neg_exn (of_int_exn 0), of_int_exn 0);
          if S.is_signed
          then (
            check "Negation (1)" S.(neg_exn (of_int_exn 10), of_int_exn (-10));
            check "Negation (2)" S.(neg_exn (of_int_exn (-10)), of_int_exn 10);
            (* Must not underflow since the negated maximum integer is representable in
               two's complement notation. *)
            check "Negation of the maximum integer" S.(neg_exn max_int, succ_exn min_int));
          if S.is_signed
          then check_raises "Negation (overflow)" (lazy S.(neg_exn min_int))
          else check_raises "Negation (underflow)" (lazy S.(neg_exn (of_int_exn 10)))
      in
      let check_abs () =
          check "Absolute" S.(abs_exn (of_int_exn 10), of_int_exn 10);
          check "Absolute of zero" S.(abs_exn (of_int_exn 0), of_int_exn 0);
          check "Absolute of the maximum integer" S.(abs_exn max_int, max_int);
          if S.is_signed
          then (
            check "Absolute" S.(abs_exn (of_int_exn (-10)), of_int_exn 10);
            check_raises "Absolute (overflow)" (lazy S.(abs_exn min_int)))
      in
      let check_add () =
          check "Addition" S.(add_exn (of_int_exn 15) (of_int_exn 20), of_int_exn 35);
          check_raises "Addition (overflow)" (lazy S.(add_exn max_int (of_int_exn 1)));
          if S.is_signed
          then
            check_raises
              "Addition (underflow)"
              (lazy S.(add_exn min_int (of_int_exn (-1))))
      in
      let check_sub () =
          check "Subtraction" S.(sub_exn (of_int_exn 20) (of_int_exn 15), of_int_exn 5);
          if S.is_signed
          then
            check_raises
              "Subtraction (overflow)"
              (lazy S.(sub_exn max_int (of_int_exn (-1))));
          check_raises "Subtraction (underflow)" (lazy S.(sub_exn min_int (of_int_exn 1)))
      in
      let check_mul () =
          check "Multiplication" S.(mul_exn (of_int_exn 12) (of_int_exn 5), of_int_exn 60);
          if S.is_signed
          then
            check
              "Negation of the maximum integer"
              S.(mul_exn max_int (of_int_exn (-1)), succ_exn min_int);
          check_raises
            "Multiplication (overflow)"
            (lazy S.(mul_exn max_int (of_int_exn 2)));
          if S.is_signed
          then (
            check_raises
              "Multiplication (overflow)"
              (lazy S.(mul_exn min_int (of_int_exn (-1))));
            check_raises
              "Multiplication (underflow)"
              (lazy S.(mul_exn min_int (of_int_exn 2)));
            check_raises
              "Multiplication (underflow)"
              (lazy S.(mul_exn max_int (of_int_exn (-2)))))
      in
      let check_div () =
          check "Division" S.(div_exn (of_int_exn 30) (of_int_exn 5), of_int_exn 6);
          check
            "Truncating division"
            S.(div_exn (of_int_exn 30) (of_int_exn 9), of_int_exn 3);
          check_raises "Division by zero" (lazy S.(div_exn (of_int_exn 1) zero));
          if S.is_signed
          then
            check_raises
              "Division (overflow)"
              (lazy S.(div_exn min_int (of_int_exn (-1))))
      in
      let check_rem () =
          check "Zero remainder" S.(rem_exn (of_int_exn 30) (of_int_exn 5), of_int_exn 0);
          check
            "A non-zero remainder"
            S.(rem_exn (of_int_exn 30) (of_int_exn 9), of_int_exn 3);
          check_raises "Division by zero" (lazy S.(rem_exn (of_int_exn 1) zero));
          if S.is_signed
          then
            check_raises
              "Remainder (overflow)"
              (lazy S.(rem_exn min_int (of_int_exn (-1))))
      in
      let check_bitwise () =
          check "Bitwise negation (flip zeroes)" S.(bit_not zero, all_ones);
          check "Bitwise negation (flip ones)" S.(bit_not all_ones, zero);
          if not S.is_signed
          then (
            let x, y = S.of_int_exn 42, S.of_int_exn 100 in
            check "Bitwise disjunction" S.(bit_or x y, of_int_exn 110);
            check "Bitwise conjunction" S.(bit_and x y, of_int_exn 32);
            check "Bitwise exclusive disjunction" S.(bit_xor x y, of_int_exn 78);
            check
              "Left shift"
              S.(
                ( shift_right_exn
                    (shift_left_exn (of_int_exn 31) (of_int_exn 3))
                    (of_int_exn 3)
                , of_int_exn 31 ));
            check
              "Right shift"
              S.(shift_right_exn (of_int_exn 212) (of_int_exn 2), of_int_exn 53))
          else (
            let x, y = S.of_int_exn (-42), S.of_int_exn 100 in
            check "Bitwise disjunction" S.(bit_or x y, of_int_exn (-10));
            check "Bitwise conjunction" S.(bit_and x y, of_int_exn 68);
            check "Bitwise exclusive disjunction" S.(bit_xor x y, of_int_exn (-78));
            check
              "Left shift"
              S.(shift_left_exn (of_int_exn 13) (of_int_exn 3), of_int_exn 104);
            check_raises
              "Left shift (underflow)"
              (lazy S.(shift_left_exn (of_int_exn 42) (of_int_exn (-1))));
            check
              "Right shift"
              S.(shift_right_exn (of_int_exn (-10)) (of_int_exn 2), of_int_exn (-3));
            check_raises
              "Right shift (underflow)"
              (lazy S.(shift_right_exn (of_int_exn 42) (of_int_exn (-1)))));
          check_raises
            "Left shift (overflow)"
            (lazy S.(shift_left_exn (of_int_exn 42) (of_int_exn (S.bits + 1))));
          check_raises
            "Right shift (overflow)"
            (lazy S.(shift_right_exn (of_int_exn 42) (of_int_exn (S.bits + 1))))
      in
      let check_cmp () =
          check_bool "Equal" S.(equal (of_int_exn 15) (of_int_exn 15), true);
          check_bool "Not equal" S.(equal (of_int_exn 15) (of_int_exn 20), false);
          check_bool "Greater than" S.(compare (of_int_exn 20) (of_int_exn 15) > 0, true);
          check_bool "Less than" S.(compare (of_int_exn 15) (of_int_exn 20) < 0, true);
          check_bool
            "Equal (compare)"
            S.(compare (of_int_exn 15) (of_int_exn 15) = 0, true)
      in
      let check_min_max () =
          check "Minimum" S.(min (of_int_exn 10) (of_int_exn 15), of_int_exn 10);
          check
            "Minimum (reversed)"
            S.(min (of_int_exn 15) (of_int_exn 10), of_int_exn 10);
          check "Maximum" S.(max (of_int_exn 10) (of_int_exn 15), of_int_exn 15);
          check
            "Maximum (reversed)"
            S.(max (of_int_exn 15) (of_int_exn 10), of_int_exn 15)
      in
      check_succ_pred ();
      check_neg ();
      check_abs ();
      check_add ();
      check_sub ();
      check_mul ();
      check_div ();
      check_rem ();
      check_bitwise ();
      check_cmp ();
      check_min_max ())
;;

let cases = ("Perform the arithmetic", perform_ops) :: cases

let show_and_parse_ints () =
    List.iter2
      (fun (min_int_s, max_int_s) (module S : S) ->
         let check_parse msg (x, s) =
             Alcotest.(check' (module S)) ~msg ~actual:(S.of_string_exn s) ~expected:x
         in
         let check msg (x, s) =
             check_string msg (S.show x, s);
             check_parse msg (x, s)
         in
         let check_format msg (x, s) =
             [ s; String.uppercase_ascii s ]
             |> List.iter (fun s ->
               check_parse msg (S.of_int_exn x, s);
               if S.is_signed
               then check_parse (msg ^ " (negative)") (S.of_int_exn (-x), "-" ^ s))
         in
         check "Check 0" (S.zero, "0");
         check "Check 42" (S.of_int_exn 42, "42");
         if S.is_signed then check "Check -42" (S.of_int_exn (-42), "-42");
         check "Check the minimum integer" (S.min_int, min_int_s);
         check "Check the maximum integer" (S.max_int, max_int_s);
         (* TODO: test also 128-bit numbers when they are supported. *)
         if S.bits != 128
         then (
           check_format "Check binary" (42, "0b101010");
           check_format "Check octal" (42, "0o52");
           check_format "Check hexadecimal" (42, "0x2a")))
      limits
      int_modules_list
;;

let cases = ("Show & parse integers", show_and_parse_ints) :: cases

let parse_errors () =
    let cause_overflow s = String.sub s 0 (String.length s - 1) ^ "9" in
    List.iter2
      (fun (min_int_s, max_int_s) (module S : S) ->
         let check_raises msg s =
             Alcotest.check_raises msg Out_of_range (fun () -> ignore (S.of_string_exn s))
         in
         check_raises "The empty string" "";
         [ " "; "\x0c" (* form feed *); "\n"; "\r"; "\t"; "\x0b" (* vertical tab *) ]
         |> List.iter (fun padding ->
           check_raises "A left-padded integer" (padding ^ "42");
           check_raises "A right-padded integer" ("42" ^ padding));
         check_raises "An invalid character (before)" "~42";
         check_raises "An invalid character (in-between)" "4~2";
         check_raises "An invalid character (after)" "42~";
         check_raises "An overflow (too many digits)" (max_int_s ^ "0");
         check_raises "An overflow (too large)" (cause_overflow max_int_s);
         if S.is_signed then check_raises "An underflow" (cause_overflow min_int_s))
      limits
      int_modules_list
;;

let cases = ("Parse errors", parse_errors) :: cases

let split_int_128 () =
    let check msg (actual, (high, low)) =
        let expected = U64.(of_string_exn high, of_string_exn low) in
        Alcotest.(check' (pair (module U64) (module U64))) ~msg ~actual ~expected
    in
    check
      "Unsigned 128-bit to parts"
      ( U128.(split (div_exn max_int (of_int_exn 2)))
      , ("9223372036854775807", "18446744073709551615") );
    check
      "Signed 128-bit to parts"
      ( I128.(split (div_exn max_int (of_int_exn 2)))
      , ("4611686018427387903", "18446744073709551615") )
;;

let cases = ("Split 128-bit integers", split_int_128) :: cases

let cases =
    cases |> List.rev |> List.map (fun (name, f) -> Alcotest.test_case name `Quick f)
;;

let () = Alcotest.run "Unit tests" [ "Fixed-width integers", cases ]
