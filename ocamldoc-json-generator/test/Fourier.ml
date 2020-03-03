open Complex

(** Fourier Transform 

    This is the module comment for a module without an mli

    It refers to another module {!Core}
*)

(** Constant {m{% \pi%}}. *)
let pi = 4.0 *. atan 1.0

(** An auxiliary routine for [dft] and [idft]. *)
let dft_aux flag xl =
  let n = List.length xl in
  let c = copysign (2.0 *. pi /. float n) flag in
  let twf t k = exp {re= 0.0; im= c *. float (t * k)} in
  (* twiddle factor *)
  let rec one_to_n acc i =
    if i = -1 then acc else one_to_n (i :: acc) (i - 1)
  in
  let nl = one_to_n [] (n - 1) in
  (* = [0; 1; 2; ...; n-1] *)
  let calc_sum k =
    let f acc t x = add acc (mul x (twf t k)) in
    List.fold_left2 f zero nl xl
  in
  List.map calc_sum nl

(** A naive implementation of Discrete Fourier Transform (DFT).

    DFT is defined as
    {eq{%
      y_k = \sum^{N-1}_{t = 0} x_t \exp\left( -i \frac{2 \pi tk}{N} \right)
      \quad (k = 0, \dots, N-1)
    %}}

    where {m{% x_1, x_2 \dots, x_{N-1}%}} are time-domain data points,
    {m{% y_1, y_2 \dots, y_{N-1}%}} are frequency-domain data points.
*)
let dft xl = dft_aux (-1.) xl

(** A naive implementation of Inverse Discrete Fourier Transform (IDFT),
 the inverse transformation of DFT:
 {eq{%
   x_t = \sum^{N-1}_{k = 0} y_k \exp\left( i \frac{2 \pi tk}{N} \right)
   \quad (t = 0, \dots, N-1)
 %}}
*)
let idft xl =
  let inv_n = inv {re= float (List.length xl); im= 0.} in
  List.map (mul inv_n) (dft_aux 1. xl)

(** An auxiliary routine for [fft] and [ifft]. *)
let fft_aux flag xl =
  let c = copysign (2.0 *. pi) flag in
  let twf m n = exp {re= 0.; im= c *. float m /. float n} in
  (* twiddle factor *)
  let rec sep_list le lo = function
    | xe :: xo :: l ->
        sep_list (xe :: le) (xo :: lo) l
    | [] ->
        (List.rev le, List.rev lo)
    | _ ->
        failwith "# of data points must be 2^N"
  in
  let rec loop n = function
    | [xe; xo] ->
        [add xe (mul xo (twf 0 2)); add xe (mul xo (twf 1 2))]
    | xl ->
        let n' = n / 2 in
        let xle, xlo = sep_list [] [] xl in
        let yl = List.combine (loop n' xle) (loop n' xlo) in
        List.mapi (fun i (ye, yo) -> add ye (mul yo (twf i n))) (yl @ yl)
  in
  loop (List.length xl) xl

(** An implementation of Cooley-Tukey Fast Fourier Transform (FFT) algorithm.
 [dft] takes {m{% O(n^2)%}} time, but [fft] takes {m{% O(n \log n)%}} time!
 *)
let fft xl = fft_aux (-1.) xl

(** An implementation of Inverse Fast Fourier Transform (IFFT).
 [idft] takes {m{% O(n^2)%}} time, but [ifft] takes {m{% O(n \log n)%}} time!
 *)
let ifft xl =
  let inv_n = inv {re= float (List.length xl); im= 0.} in
  List.map (mul inv_n) (fft_aux 1. xl)
