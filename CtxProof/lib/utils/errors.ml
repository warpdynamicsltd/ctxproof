exception CxError of string

type kernel_error_code =
  | NotAdmissible
  | MalformedAxiom
  | UnknownAxiom
  | MalformedRule
  | NotAllowedSkolemTerm
  | RefOutOfBound
  | ImplicationFormExpected
  | ReferenceExpected
  | RuleConstraintViolation
  | AxiomViolation
  | AssumptionViolation
  | RuleViolation
  | ContextViolation
  | InvalidReference
  | UnknownProblem
  | VarExpectedInSubMap
  | DuplicateVarInSubMap

let kernel_error_message = function
  | NotAdmissible -> "not admissible"
  | MalformedAxiom -> "malformed axiom"
  | UnknownAxiom -> "unknown axiom"
  | MalformedRule -> "malformed rule"
  | NotAllowedSkolemTerm -> "not allowed skolem term"
  | RefOutOfBound -> "ref out of bound"
  | ImplicationFormExpected -> "implication form expected"
  | ReferenceExpected -> "reference expected"
  | RuleConstraintViolation -> "rule constraint violation"
  | AxiomViolation -> "axiom violation"
  | AssumptionViolation -> "assumption violation"
  | RuleViolation -> "rule violation"
  | ContextViolation -> "context violation"
  | InvalidReference -> "invalid reference"
  | UnknownProblem -> "problem"
  | VarExpectedInSubMap -> "expected Var in sub_map"
  | DuplicateVarInSubMap -> "duplicate variable in sub_map"
