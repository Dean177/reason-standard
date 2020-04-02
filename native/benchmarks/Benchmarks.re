open Core;
open Core_bench;
open Standard;

let () =
  Command.run(
    Bench.make_command([
      Bench.Test.create_indexed(
        ~module_name="Vector",
        ~name="initialize",
        ~args=[100, 1_000, 10_000, 50_000, 100_000, 500_000, 1_000_000],
        length => {
        Core.Staged.stage(() =>
          ignore(Vector.initialize(length, ~f=Fun.constant(0)))
        )
      }),
      Bench.Test.create_indexed(
        ~module_name="Vector",
        ~name="get",
        ~args=[100, 1_000, 10_000, 50_000, 100_000, 500_000, 1_000_000],
        length => {
          let vector = Vector.initialize(length, ~f=Fun.constant(0));
          Core.Staged.stage(() => ignore(Vector.get(vector, 50)));
        },
      ),
      Bench.Test.create_indexed(
        ~module_name="Vector",
        ~name="set",
        ~args=[100, 1_000, 10_000, 50_000, 100_000, 500_000, 1_000_000],
        length => {
          let vector = Vector.initialize(length, ~f=Fun.constant(0));
          Core.Staged.stage(() => ignore(Vector.set(vector, 50, 1)));
        },
      ),
      Bench.Test.create_indexed(
        ~module_name="Vector",
        ~name="push",
        ~args=[100, 1_000, 10_000, 50_000, 100_000, 500_000, 1_000_000],
        length => {
          let vector = Vector.initialize(length, ~f=Fun.constant(0));
          Core.Staged.stage(() => ignore(Vector.push(vector, 0)));
        },
      ),
    ]),
  );