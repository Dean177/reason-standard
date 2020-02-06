open Jest;
open Expect;

module Eq: {
  type t('a);
  let bool: t(bool);
  let char: t(char);
  let int: t(int);
  let float: t(float);
  let string: t(string);
  let array: (t('a)) => t(array('a));
  let list: (t('a)) => t(list('a));
  let option: (t('a)) => t(option('a));
  let result: (t('ok), t('error)) => t(Standard.Result.t('ok, 'error));
  let pair: (t('a), t('b)) => t(('a, 'b));
  let trio: (t('a), t('b), t('c)) => t(('a, 'b, 'c));
} = {
  type t('a) = ('a, 'a) => unit
  let ignore = (_) => Standard.Fun.ignore;
  let bool = ignore;
  let char = ignore;
  let int = ignore;
  let float = ignore;
  let string = ignore;
  let array = (_) => ignore;
  let list = (_) => ignore;
  let option = (_) => ignore;
  let result = (_, _) => ignore;
  let pair = (_,_) => ignore;
  let trio = (_,_, _) => ignore;
}

let suite = describe;

let describe = describe;

let test = test;

let testAll = testAll;

let expect = expect;

let toEqual = (_: Eq.t('a), value: 'a) => toEqual(value);

let toBeTrue = () => toBe(true);

let toBeFalse = () => toBe(false);

let toBeCloseTo = toBeCloseTo;

let toRaise = (_exn) => toThrow;

let toThrow = toThrow;