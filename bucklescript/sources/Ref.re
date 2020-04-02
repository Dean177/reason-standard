type t('a) = ref('a)

let decrement = (reference: ref(int)): unit => {
  reference := reference^ - 1;
};

let increment = (reference: ref(int)): unit => {
  reference := reference^ + 1;
};