#!r6rs
(library (xitomatl sxml-tools xpath-ast (2008 06 27))
  (export
    txp:ast-operation-helper
    txp:ast-params
    txp:ast-res
    txp:ast-api-helper
    txp:xpath->ast
    txp:xpointer->ast
    txp:expr->ast
    txp:sxpath->ast
    txp:step?
    txp:step-axis
    txp:step-node-test
    txp:step-preds
    txp:construct-step)
  (import
    (rnrs)
    (xitomatl include)
    (xitomatl srfi and-let*)
    (xitomatl sxml-tools xpath-parser)
    (xitomatl ssax private-5-1 output))
                
  (include/resolve ("xitomatl" "sxml-tools") "xpath-ast.scm")
)
