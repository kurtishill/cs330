#
# Class Interpreter - Type Checker
#

module CITypes # based on the CI3 interpreter

push!(LOAD_PATH,pwd())

using Error
using Lexer
export parse, type_of_expr, NumType, BoolType, FuncType, NListType

# ===================================================

abstract type AE
end

abstract type TypeVal
end

# <expr> ::= <number>
type NumNode <: AE
  n::Real
end

# <expr> ::= true
# <expr> ::= false
type BooleanNode <: AE
  v::Bool
end

# <expr> ::= (+ <expr> <expr>)
type PlusNode <: AE
    lhs::AE
    rhs::AE
end

# <expr> ::= (- <expr> <expr>)
type MinusNode <: AE
    lhs::AE
    rhs::AE
end

# <expr> ::= (iszero <expr>)
type IsZeroNode <: AE
  arg::AE
end

# <expr> ::= (ifb <expr> <expr> <expr>)
type IfBNode <: AE
    cond::AE
    zerobranch::AE
    nzerobranch::AE
end

# <expr> ::= (with <id> <expr> <expr>)
type WithNode <: AE
    sym::Symbol
    binding_expr::AE
    body::AE
end

# <expr> ::= <id>
type VarRefNode <: AE
    sym::Symbol
end

# <expr> ::= (lambda <id> : <type> <expr>)
type FuncDefNode <: AE
  formal_parameter::Symbol
  formal_type::TypeVal
  body::AE
end

# <expr> ::= (<expr> <expr>)
type FuncAppNode <: AE
    fun_expr::AE
    arg_expr::AE
end

# <expr> ::= nempty
type NEmptyNode <: AE
end

# <expr> ::= (nisempty <expr>)
type NIsEmptyNode <: AE
  list::AE
end

# <expr> ::= (ncons <expr> <expr>)
type NConsNode <: AE
  f::AE
  r::AE
end

# <expr> ::= (nfirst <expr>)
type NFirstNode <: AE
  list::AE
end

# <expr> ::= (nrest <expr>)
type NRestNode <: AE
  list::AE
end

# ===================================================

# <type> ::= number
type NumType <: TypeVal
end

# <type> ::= boolean
type BoolType <: TypeVal
end

# <type> ::= (<type> : <type>)
type FuncType <: TypeVal
  arg_type::TypeVal
  result_type::TypeVal
end

# <type> ::= nlist
type NListType <: TypeVal
end

# ===================================================

abstract type TypeEnvironment
end

type EmptyTypeEnv <: TypeEnvironment
end

type ExtendedTypeEnv <: TypeEnvironment
    sym::Symbol
    val::TypeVal
    parent::TypeEnvironment
end

# ===================================================

# Parser for expressions
# Functional for valid input, doesn't fully reject bad input

function parse( expr::Number )
    return NumNode( expr )
end

function parse( expr::Bool )
  return BooleanNode( expr )
end

function parse( expr::Symbol )
  if expr == :nempty
    return NEmptyNode()
  else
    return VarRefNode( expr )
  end
end

function parse( expr::Array{Any} )

  op_symbol = expr[1]

  if op_symbol == :+
    lhs = parse( expr[2] )
    rhs = parse( expr[3] )
    return PlusNode( lhs, rhs )

  elseif op_symbol == :-
    lhs = parse( expr[2] )
    rhs = parse( expr[3] )
    return MinusNode( lhs, rhs )

  elseif op_symbol == :iszero
    arg = parse( expr[2] )
    return IsZeroNode( arg )

  elseif op_symbol == :ifb
    condition = parse( expr[2] )
    true_branch = parse( expr[3] )
    false_branch = parse( expr[4] )
    return IfBNode( condition, true_branch, false_branch )

  elseif op_symbol == :with
    sym = expr[2]
    binding_expr = parse( expr[3] )
    body = parse( expr[4] )
    return WithNode( sym, binding_expr, body )

  elseif op_symbol == :lambda
    formal = expr[2]
    formal_type = parse_type(expr[4])
    body = parse(expr[5])
    return FuncDefNode( formal, formal_type, body )

  elseif op_symbol == :ncons
    f = parse(expr[2])
    r = parse(expr[3])
    return NConsNode( f, r )

  elseif op_symbol == :nisempty
    list = parse(expr[2])
    return NIsEmptyNode( list )

  elseif op_symbol == :nfirst
    list = parse(expr[2])
    return NFirstNode( list )

  elseif op_symbol == :nrest
    list = parse(expr[2])
    return NRestNode( list )

  else
    return FuncAppNode( parse(expr[1]), parse(expr[2]) )

  end
end

function parse( expr::Any )
  throw( LispError("Invalid expression $expr") )
end

# ===================================================

# Parser for type expressions

function parse_type( t::Symbol )
  if (t == :number)
    return NumType()
  elseif (t == :boolean)
    return BoolType()
  elseif (t == :nlist)
    return NListType()
  end
end

function parse_type( t :: Array{Any} )
  return FunType( parse_type(t[1]),
                  parse_type(t[3]))
end

function parse_type( expr::Any )
  throw( LispError("Invalid type $expr") )
end

# ===================================================

# Type checking functions (modeled after the earlier calc)

function type_of_expr( ast::AE )
  return type_of_expr( ast, EmptyTypeEnv() )
end

function type_of_expr( ast::NumNode, env::TypeEnvironment )
  return NumType()
end

function type_of_expr( ast::BooleanNode, env::TypeEnvironment )
  return BoolType()
end

function type_of_expr( ast::PlusNode, env::TypeEnvironment )
  left = type_of_expr( ast.lhs, env )
  right = type_of_expr( ast.rhs, env )
  return type_of_math_expr( left, right )
end

function type_of_expr( ast::MinusNode, env::TypeEnvironment )
  left = type_of_expr( ast.lhs, env )
  right = type_of_expr( ast.rhs, env )
  return type_of_math_expr( left, right )
end

function type_of_expr( ast::IsZeroNode, env::TypeEnvironment )
    arg = type_of_expr( ast.arg, env )
    if arg == NumType()
        return BoolType()
    else
        throw( LispError("Invalid value for IsZeroNode"))
    end
end

function type_of_expr( ast::VarRefNode, env::TypeEnvironment )
    return env.val
end

function type_of_expr( ast::IfBNode, env::TypeEnvironment )
    if type_of_expr( ast.cond ) == BoolType()
        zb = type_of_expr( ast.zerobranch, env )
        nzb = type_of_expr( ast.nzerobranch, env )
        if same_type( zb, nzb )
            return typeof( zb )
        else
            throw( LispError("Types of the branches in IfBNode are not equal") )
        end
    else
        throw( ListError("Condition of IfBNode is not of type Bool") )
    end
end

function type_of_expr( ast::WithNode, env::TypeEnvironment )
    type_binding_expr = type_of_expr( ast.binding_expr, env )
    newEnv = ExtendedTypeEnv( ast.sym, type_binding_expr, env )
    return type_of_expr( ast.body, newEnv )
end

function type_of_expr( ast::FuncDefNode, env::TypeEnvironment )
    newEnv = ExtendedTypeEnv( ast.formal_parameter, ast.formal_type, env )
    return FuncType( ast.formal_type, type_of_expr( ast.body, newEnv ) )
end

function type_of_expr( ast::FuncAppNode, env::TypeEnvironment )
    funcTypes = FuncType( type_of_expr( ast.fun_expr, env ) )
    t1 = funcTypes.arg_type
    t2 = funcTypes.result_type
    if same_type( t1, type_of_expr( ast.arg_expr, env ) )
        return t2
    else
        throw( LispError( "Invalid Function Application types" ) )
    end
    return type_of_expr( ast.arg_expr, env )
end

function type_of_expr( ast::NEmptyNode, env::TypeEnvironment )
    return NListType()
end

function type_of_expr( ast::NIsEmptyNode, env::TypeEnvironment )
    val = type_of_expr( ast.list, env )
    if same_type( val, NListType() )
        return BoolType()
    else
        throw( LispError( "NIsEmptyNode doesn't contain a list" ) )
    end
end

function type_of_expr( ast::NConsNode, env::TypeEnvironment )
    if same_type( type_of_expr( ast.f, env ), NumType() ) && same_type( type_of_expr( ast.r, env ), NListType() )
        return NListType()
    else
        throw( LispError( "Incorrect types in NConsNode" ) )
    end
end

function type_of_expr( ast::NFirstNode, env::TypeEnvironment )
    if same_type( type_of_expr( ast.list, env ), NListType() )
        return NumType()
    else
        throw( LispError( "Invalid type of first element in NFirstNode" ) )
    end
end

function type_of_expr( ast::NRestNode, env::TypeEnvironment )
    return NListType()
end

# the rest of your type-checking functions go here...

# ===================================================

# Helper function for comparing two type values recursively if necessary

same_type( t1::FuncType, t2::FuncType ) =
    (same_type( t1.arg_type, t2.arg_type )
  && same_type( t1.result_type, t2.result_type ))

same_type{ T <: TypeVal }( t1::T, t2::T ) = true

same_type( t1::TypeVal, t2::TypeVal ) = false

# ===================================================

# Type judgments (could be folded into type checking functions)

function type_of_math_expr( left::NumType, right::NumType )
  return NumType()
end

function type_of_math_expr( left::Any, right::Any )
  throw( LispError("Operands for + or - must be numbers") )
end

# the rest of your type-judgement functions (if you choose to separate them) go here...

# ===================================================

# convenience function to make everything easier
function type_of_expr( expr::AbstractString )
  return type_of_expr( parse( Lexer.lex(expr) ) )
end

# evaluate a series of tests in a file
function typef( fn::AbstractString )
  f = open( fn )

  cur_prog = ""
  for ln in eachline(f)
      ln = chomp( ln )
      if length(ln) == 0 && length(cur_prog) > 0
          println( "" )
          println( "--------- Evaluating ----------" )
          println( cur_prog )
          println( "---------- Returned -----------" )
          try
              println( type_of_expr( cur_prog ) )
          catch errobj
              println( ">> ERROR: lxd" )
              lxd = Lexer.lex( cur_prog )
              println( lxd )
              println( ">> ERROR: ast" )
              ast = parse( lxd )
              println( ast )
              println( ">> ERROR: rethrowing error" )
              throw( errobj )
          end
          println( "------------ done -------------" )
          println( "" )
          cur_prog = ""
      else
          cur_prog *= ln
      end
  end

  close( f )
end

function lexParse(str)
  CITypes.parse(Lexer.lex(str))
end

function parseInter(str)
  CITypes.type_of_expr(lexParse(str), EmptyTypeEnv())
end

# ===================================================

end # module
