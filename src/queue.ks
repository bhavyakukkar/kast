module:

const Queue = (
    module:

    const t = [T] newtype {
        .front_stack :: ArrayList.t[T],
        .back_stack :: ArrayList.t[T],
    };

    const new = [T] () -> Queue.t[T] => {
        .front_stack = ArrayList.new(),
        .back_stack = ArrayList.new(),
    };

    const push = [T] (q :: &mut Queue.t[T], value :: T) => (
        &mut q^.back_stack |> ArrayList.push_back(value);
    );

    const pop = [T] (q :: &mut Queue.t[T]) -> Option.t[T] => (
        if &q^.front_stack |> ArrayList.length == 0 then (
            while &q^.back_stack |> ArrayList.length != 0 do (
                &mut q^.front_stack |> ArrayList.push_back(&mut q^.back_stack |> ArrayList.pop_back);
            );
        );
        if &q^.front_stack |> ArrayList.length == 0 then (
            :None
        ) else (
            :Some (&mut q^.front_stack |> ArrayList.pop_back)
        )
    );
);
