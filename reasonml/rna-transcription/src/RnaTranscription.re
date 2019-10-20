type dna =
  | A
  | C
  | G
  | T;

type rna =
  | A
  | C
  | G
  | U;

let toRna = xs =>
  List.map(
    (x: dna) =>
      switch (x) {
      | G => C
      | C => G
      | T => A
      | A => U
      },
    xs,
  );