module:

const Token = (
    module:
    
    const print_impl = () => (
        Token.Shape.print_impl();
    );
    
    const print = () => print_impl();
    
    const Shape = (
        module:
        
        const print_impl = () => (
            @native "console.log(123)"
        );
    );
);