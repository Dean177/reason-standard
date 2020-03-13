(lang dune 2.4)
(name reason-standard)
(generate_opam_files true)

(source (github dean177/reason-standard))
(license MIT)
(authors "Dean Merchant")
(maintainers "deanmerchant@gmail.com")

(package
 (name reason-standard) 
 (synopsis "A portable standard library enhancement for Reason and OCaml.")
 (description "A portable standard library enhancement for Reason and OCaml.")
 (depends
  (alcotest :with-test)
  (base (>= 0.13.0))  
  (zarith (>= 1.9.1))  
  (dune (>= 2.4))  
  (reason (>= 3.4.0))  
 )
)