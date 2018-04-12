module TransInt

push!(LOAD_PATH, ".")

using Error
using Lexer
export parse, calc, analyze, NumVal, ClosureVal

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

arithops = Dict(:+ => +, :- => -, :* => *, :/ => /, :mod => mod, :collatz => collatz)
logicops = Dict(:if0 => "if0", :with => "with", :lambda => "lambda", :and => "and")

#-------------------------------------------------------

abstract type AE # arithmetic expression
end

# <AE> ::= <number>
type NumNode <: AE
    n::Real
end

type Unop <: AE
    op::Function
    rhs::AE
end

type BinopNode <: AE
    op::Function
    lhs::AE
    rhs::AE
end

type PlusNode <: AE
    exprs::Array{AE}
end

type And <: AE
    exprs::Array{AE}
end

type If0Node <: AE
    condition::AE
    zero_branch::AE
    nonzero_branch::AE
end

type WithNode <: AE
    forActs::Dict
    body::AE
end

type VarRefNode <: AE
    sym::Symbol
end

type FuncDefNode <: AE # function definition
    formals::Array{Symbol}
    fun_body::AE
end

type FuncAppNode <: AE # function application
    fun_expr::AE
    arg_expr::Array{AE} # actual parameter
end

#------------------------------------------------------

abstract type RetVal
end

abstract type Environment
end

type NumVal <: RetVal
    n::Real
end

type ClosureVal <: RetVal
    formals::Array{Symbol}
    body::AE
    env::Environment
end

#------------------------------------------------------

type EmptyEnv <: Environment
end

type ExtendedEnv <: Environment
    sym::Array{Symbol}
    val::Array{RetVal}
    parent::Environment
end

#------------------------------------------------------

function parse( expr::Number )
    return NumNode( expr )
end

function parse( expr::Symbol )
    if haskey( arithops, expr ) || haskey( logicops, expr )
        throw( LispError( "Can't use keyword" ) )
    end
    return VarRefNode( expr )
end

function parse( expr::Array{Any} )

    if expr[1] == :collatz
        if length( expr ) == 2
            return Unop( arithops[expr[1]], parse( expr[2] ) )
        else
            throw( LispError("Too many arguments") )
        end

    elseif expr[1] == :-
        if length( expr ) == 2
            return Unop( arithops[expr[1]], parse( expr[2] ) )
        elseif length( expr ) == 3
            return BinopNode( arithops[expr[1]], parse( expr[2] ), parse( expr[3] ) )
        else
            throw( LispError("Too many arguments") )
        end

    elseif expr[1] == :+
        if length( expr ) < 3
            throw( LispError( "Invalid num arguments" ) )
        end
        arr = Array{AE}(0);
        i = 2
        while i <= length( expr )
            push!( arr, parse( expr[i] ) )
            i += 1
        end
        return PlusNode( arr )

    elseif haskey(arithops, expr[1])
        if length( expr ) == 3
            return BinopNode( arithops[expr[1]], parse( expr[2] ), parse( expr[3] ) )
        else
            throw( LispError("Too many arguments") )
        end

    else
        if expr[1] == :if0
            if length( expr ) == 4
                return If0Node( parse(expr[2]), parse(expr[3]) , parse(expr[4]) )
            else
                throw( LispError( "Invalid num arguments" ) )
            end

        elseif expr[1] == :and
            if length( expr ) < 3
                throw( LispError( "Invalid num arguments" ) )
            end
            arr = Array{AE}(0)
            i = 2
            while i <= length( expr )
                push!( arr, parse( expr[i] ) )
                i += 1
            end
            return And( arr )

        elseif expr[1] == :with
            d = Dict()
            i = 1
            if !(expr[2] isa Array)
                throw( LispError( "Invalid syntax" ) )
            end
            while i <= length( expr[2] )
                j = 1
                if !(expr[2][i] isa Array)
                    throw( LispError( "invalid syntax" ) )
                end
                while j < length( expr[2][i] )
                    if length( expr[2][i] ) < 2
                        throw( LispError( "Invalid num arguments" ) )
                    end
                    if haskey( arithops, expr[2][i][j] ) || haskey( logicops, expr[2][i][j] )
                        throw( LispError( "Can't use keywords" ) )
                    end
                    if haskey( d, ( expr[2] )[i][j] )
                        throw( LispError( "Duplicate variables" ) )
                    end
                    if !( (expr[2] )[i][j] isa Symbol)
                        throw( LispError("Invalid syntax" ) )
                    end
                    d[ ( expr[2] )[i][j] ] = parse( (expr[2] )[i][j+1])
                    j += 1
                end
                i += 1
            end
            return WithNode( d, parse( expr[end] ) )

        elseif expr[1] == :lambda
            if length( expr ) != 3
                throw( LispError( "Invalid num arguments" ) )
            end
            i = 1
            tmp = []
            if !(expr[2] isa Array)
                throw( LispError( "Invalid syntax" ) )
            end
            while i <= length( expr[2] )
                if expr[2][i] in tmp
                    throw( LispError( "Duplicate variable" ) )
                end
                if haskey( arithops, expr[2][i] ) || haskey( logicops, expr[2][i] )
                    throw( LispError( "Can't use keywords" ) )
                end
                if !(expr[2][i] isa Symbol)
                    throw( LispError( "Invalid syntax" ) )
                end
                push!( tmp, expr[2][i] )
                i += 1
            end

            return FuncDefNode( tmp, parse( expr[3] ) )

        else
            tmp = Array{AE}(0)
            i = 2
            if length( expr ) == 1
                return FuncAppNode( parse( expr[1] ), tmp )
            else
                while i <= length( expr )
                    push!( tmp, parse( expr[i] ) )
                    i += 1
                end
            end
            return FuncAppNode( parse( expr[1] ), tmp )
        end
    end

    throw( LispError("Unknown operator!") ) # SHOULD NEVER GET HERE
end

function parse( expr::Any )
    throw( LispError("Invalid type $expr") )
end

#--------------------------------------------------------

function analyze( ast::NumNode ) # no sub expressions, so you don't need to recurse
    return ast
end

function analyze( ast::VarRefNode )
    return ast
end

function analyze( ast::PlusNode )
    if length( ast.exprs ) == 2
        #alhs = ast.exprs[1];
        #arhs = ast.exprs[2];
        #if typeof( alhs ) == NumNode && typeof( arhs ) == NumNode
        #    return NumNode( alhs.n + arhs.n )
        #end
        return BinopNode( +, analyze( ast.exprs[1] ), analyze( ast.exprs[2] ) )
    else
        lhs = analyze( ast.exprs[end - 1] )
        rhs = analyze( ast.exprs[end] )
        pop!( ast.exprs )
        pop!( ast.exprs )
        node = BinopNode( +, lhs, rhs )
        push!( ast.exprs, node )
        return analyze( PlusNode( ast.exprs ) )
    end
end

function analyze( ast::Unop )
    arhs = analyze( ast.rhs )
    #if typeof( arhs ) == NumNode
    #    return NumNode( ast.op( arhs.n ) )
    #end

    return Unop( ast.op, arhs )
end

function analyze( ast::BinopNode )
    alhs = analyze( ast.lhs )
    arhs = analyze( ast.rhs )
    #if typeof(alhs) == NumNode && typeof(arhs) == NumNode
    #    return NumNode( ast.op( alhs.n, arhs.n ) )
    #end

    return BinopNode( ast.op, alhs, arhs )
end

function analyze( ast::WithNode )
    arrFormals = Array{Symbol}(0);
    arrActuals = Array{AE}(0);
    for (k,v) in ast.forActs
        push!( arrFormals, k )
        push!( arrActuals, analyze( v ) )
    end
    fdn = FuncDefNode( arrFormals, analyze( ast.body ) )
    return FuncAppNode( fdn, arrActuals )
end

function analyze( ast::If0Node )
    acond = analyze( ast.condition )

    #if typeof( acond ) == NumNode
    #    if acond.n == 0
    #        return analyze( ast.zero_branch )
    #    else
    #        return analyze( ast.nonzero_branch )
    #    end
    #end

    azb = analyze( ast.zero_branch )
    anzb = analyze( ast.nonzero_branch )
    return If0Node( acond, azb, anzb )
end

function analyze( ast::And )
    if length( ast.exprs ) == 1
        return If0Node( analyze( ast.exprs[1] ), NumNode(0), NumNode(1) )
    else
        return If0Node( analyze( ast.exprs[1] ), NumNode(0), analyze( And( ast.exprs[2:end] ) ) )
    end
end

function analyze( ast::FuncDefNode )
    return FuncDefNode( ast.formals, analyze( ast.fun_body ) )
end

function analyze( ast::FuncAppNode )
    arr = Array{AE}(0)
    for x in ast.arg_expr
        push!( arr, analyze( x ) )
    end
    return FuncAppNode( analyze( ast.fun_expr), arr )
end

#--------------------------------------------------------

function executeUnop( op::Function, expr::AE, env::Environment )
    v = calc( expr, env )
    if typeof(v) == NumVal
        tmp = op( v.n )
        return NumVal( tmp )
    else
        throw( LispError( "Incorrect value" ) )
    end
end

function executeBinop( op::Function, lhs::AE, rhs::AE, env::Environment )
    l = calc( lhs, env )
    r = calc( rhs, env )
    if typeof(l) == NumVal && typeof(r) == NumVal
        tmp = op( l.n, r.n )
        return NumVal( tmp )
    else
        throw( LispError( "Incorrect values" ) )
    end
end

function calc( ast::AE )
    return calc( ast, EmptyEnv() )
end

function calc( ast::NumNode, env::Environment )
    return NumVal( ast.n )
end

function calc( ast::Unop, env::Environment )
    if calc( ast.rhs, env ) == ClosureVal
        throw( LispError( "invalid semantics" ) )
    end
    if ast.op == collatz
        if calc( ast.rhs, env ).n <= 0
            throw( LispError("Invalid value") )
        else
            executeUnop( ast.op, ast.rhs, env )
        end
    else
        executeUnop( ast.op, ast.rhs, env )
    end
end


function calc( ast::BinopNode, env::Environment )
    if ast.op == /
        if calc( ast.rhs, env ).n == 0
            throw( LispError("Invalid value") )
        end
    end
    return executeBinop( ast.op, ast.lhs, ast.rhs, env )
end

function calc( ast::If0Node, env::Environment )
    condition = calc( ast.condition, env )
    if typeof(condition) != NumVal
        throw( LispError( "Invalid conditional expression" ) )
    end
    if condition.n == 0
        return calc( ast.zero_branch, env )
    else
        return calc( ast.nonzero_branch, env )
    end
end

#function calc( ast::WithNode, env::Environment )
#    tmp1 = Array{Symbol}(0)
#    tmp2 = Array{RetVal}(0)
#    for (k,v) in ast.forActs
#        push!( tmp1, k )
#        push!( tmp2, calc( v, env ) )
#    end
#    ext_env = ExtendedEnv( tmp1, tmp2, env )
#    return calc( ast.body, ext_env )
#end

function calc( ast::VarRefNode, env::EmptyEnv )
    throw( Error.LispError("Undefined variable " * string( ast.sym )) )
end

function calc( ast::VarRefNode, env::ExtendedEnv )
    if ast.sym in env.sym
        i = 1
        while i <= length( env.sym )
            if env.sym[i] == ast.sym
                break
            else
                i += 1
            end
        end
        return env.val[i]
    else
        return calc( ast, env.parent )
    end
end

function calc( ast::FuncDefNode, env::Environment )
    return ClosureVal( ast.formals, ast.fun_body , env )
end

function calc( ast::FuncAppNode, env::Environment )
    closure_val = calc( ast.fun_expr, env )
    if typeof( closure_val ) != ClosureVal
        throw( LispError( "Invalid type" ) )
    end
    actual_parameters = calc( ast.arg_expr, env )
    if length( closure_val.formals ) != length( actual_parameters )
        throw( LispError( "Invalid num parameters" ) )
    end
    ext_env = ExtendedEnv( closure_val.formals,
                           actual_parameters,
                           closure_val.env )
    if typeof( actual_parameters ) == ClosureVal
        actual_parameters.env = ext_env
    end
    return calc( closure_val.body, ext_env )
end

function calc( ast::Array{AE}, env )
    i = 1
    tmp = Array{RetVal}(0)
    while i <= length( ast )
        push!( tmp, calc( ast[i], env ) )
        i += 1
    end
    return tmp
end

#--------------------------------------------------------

function interp( cs::AbstractString )
    lxd = Lexer.lex( cs )
    ast = parse( lxd )
    revised_ast = analyze( ast )
    return calc( revised_ast, EmptyEnv() )
end

function parseT(str::AbstractString)
    ast = Lexer.lex(str)
    ExtInt.parse(ast)
end

function interpT(str::AbstractString)
    ExtInt.calc(parseT(str))
end



end #module
