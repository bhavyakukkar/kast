const PanicHandlerT = newtype {
    .handle :: [T] String -> T,
};
const PanicHandler = @context PanicHandlerT;

const default_panic_handler :: PanicHandlerT = {
    .handle = [T] (s :: String) -> T => @cfg (
        | target.name == "interpreter" => (@native "panic")(s)
        | target.name == "javascript" => (@native "Kast.panic")(s)
    ),
};

const panic = [T] (s :: String) -> T => (
    (@current PanicHandler).handle(s)
);