module RudInt

push!(LOAD_PATH, ".")

using Error
using Lexer
export parse, calc

function collatz( n::Real )
    return collatz_helper( n, 0 )
end

function collatz_helper( n::Real, num_iters::Int )
    if n == 1
        return num_iters
    end
    if mod(n,2)==0
        return collatz_helper( n/2, num_iters+1 )
    else
        return collatz_helper( 3*n+1, num_iters+1 )
    end
end

d = Dict(:+ => +, :- => -, :* => *, :/ => /, :mod => mod, :collatz => collatz)

#-------------------------------------------------------

abstract type AE # arithmetic expression
end

# <AE> ::= <number>
type Num <: AE
    n::Real
end

type Unop <: AE
    op::Function
    rhs::AE
end

type Binop <: AE
    op::Function
    lhs::AE
    rhs::AE
end

#------------------------------------------------------

function parse( expr::Number ) # make abstract piece of syntax
    return Num( expr )
end

function parse( expr::Array{Any} )

    if expr[1] == :collatz
        if length( expr ) == 2
            return Unop( d[expr[1]], parse( expr[2] ) )
        else
            throw( LispError("Too many arguments") )
        end

    elseif expr[1] == :-
        if length( expr ) == 2
            return Unop( d[expr[1]], parse( expr[2] ) )
        elseif length( expr ) == 3
            return Binop( d[expr[1]], parse( expr[2] ), parse( expr[3] ) )
        else
            throw( LispError("Too many arguments") )

        end
    elseif haskey(d, expr[1])
        if length( expr ) == 3
            return Binop( d[expr[1]], parse( expr[2] ), parse( expr[3] ) )
        else
            throw( LispError("Too many arguments") )
        end
    end

    throw( LispError("Unknown operator!") )
end

function parse( expr::Any )
    throw( LispError("Invalid type $expr") )
end

#--------------------------------------------------------

function calc( ast::Num )
    return ast.n
end

function calc( ast::Unop )
    if ast.op == collatz
        if calc( ast.rhs ) <= 0
            throw( LispError("Invalid value") )
        else
            ast.op( calc( ast.rhs ) )
        end
    else
        ast.op( calc( ast.rhs ) )
    end
end


function calc( ast::Binop )
    if ast.op == /
        if calc( ast.rhs ) == 0
            throw( LispError("Invalid value") )
        end
    end
    return ast.op( calc( ast.lhs ), calc( ast.rhs ) )
end

#--------------------------------------------------------

function interp( cs::AbstractString )
    lxd = Lexer.lex( cs )
    ast = parse( lxd )
    return calc( ast )
end


end #module
