use (import "./token.ks").*;

module:

const TokenStream = (
    module:

    const t = newtype {
        .index :: Int32,
        .prev :: Option.t[Token.t],
        .peeked :: Token.t,
        .next :: () -> Token.t,
    };
    
    const from_fn = (f :: () -> Token.t) -> TokenStream.t => {
        .index = 0,
        .prev = :None,
        .peeked = f(),
        .next = f,
    };

    const prev = (self :: &TokenStream.t) -> Option.t[Token.t] => (
        self^.prev
    );
    
    const peek = (self :: &TokenStream.t) -> Token.t => (
        self^.peeked
    );
    
    const advance = (self :: &mut TokenStream.t) => (
        self^.prev = :Some self^.peeked;
        self^.index += 1;
        self^.peeked = (self^.next)();
    );
);
