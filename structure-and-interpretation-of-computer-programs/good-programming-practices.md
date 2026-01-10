### Code layout and design
When design a computational system it is extremely valuable to decide what kinds of information naturally should be grouped together and to then create structures that perform that grouping while maintaining interfaces to the structures that hide the details.
A key stage in designing a computational system is determining the natural data structures of the system.
A second stage in designing a computational system is how to best break the computation into modules or pieces.
- Is there part of the problem that defines a computation that is likely to be used many times?
- Are there parts of the problem that can be conceptualized in terms of their behaviour?
- How they convert certain inputs into certain types of outputs without worrying about the details of how it is done?
- Does this help us focus on other parts of the computation?
- Can one identify parts of the computation in terms of their role and think about that role in the overall computation without having to know details of the actual computation?
Finally, given that one can identify data structures whose information is to be manipulated and stages of computation in which that information is transformed one wants to decide the overall flow of information between the modules.
- What types of inputs does each module need?
- What types of data does each model return?
- How does one ensure that the correct types are provided in the correct order?

- Data structures
    - Natural collections of information
    - Supression of detail from use of data
- Procedural modules
    - Computation to be reused
    - Supression of detail from use of procedure
- Interfaces
    - "Types" of inputs and outputs

### Documenting code
Supporting code maintenance. Can you read your code a year after writing it and still understand why you made particular design decisions?

1. Documenting code
    - Description of input/output behaviours
    - Expected or required types of arguments
    - Type of returned value
    - List of constraints that must be satisfied by arguments or stages of computation
    - Expected state of computation at key points in code

```cl
    ;;; Compute approximate square root by successive refinement, guess is current approximation, x is the number whose square root we are seeking.
    ;; Type: (number, number) -> number
    ; Constraints: guess^2 == x
    (defun sqrt-helper (x guess)
        (if (not (>= x 0))
            (error "Not a positive number")
            (if (or (not (number-p x))
                (not (number-p guess)))
                (error "Report this somehow")
                (if (good-enough-p X guess) ; Can we stop?
                    guess                   ; If yes, return.
                    (sqrt-helper x          ; If not, then get better guess and repeat process.
                                (improve x guess))))))
```

### Debugging errors
- Common sources of errors
    - Unbound variable
        - Cause: Typo
        - Solution: Search for instance

        - Cause: Reference outside scope of binding
        - Solution:
            - Search for instance
            - Use debugging tools to isolate instance
    - Syntax errors
        - Wrong number of arguments
            - Source: Programming error
            - Solution: Use debugger to isolate instance
        - Type errors
            - As procedure
            - As arguments
                - Source: Calling error
                - Solution: Trace back through chain of calls
    - Structure Errors
        - Wrong initialization of parameters
        - Wrong base case
        - Wrong end test...
        - ... and so on...
    - Evaluation and verification
        - Choosing good test cases
            - Pick values for input parameters as limits of legal range
                - Base case of recursive procedure
            - Pick values that span legal range of parameters
        - Retest prior cases after making code changes
- Common tools to debug
    - The debugger
        - Places user inside state of computation at time of error
        - Can step through
            - Reductions
            - Substitutions
        - Can examine bindings of variables and parameters
    - The ubiquitous print/display expression
    - Tracing
    - Stepping

- Display parameters to isolate errors
- Test cases to highlight errors
- Check range of test cases

### Using types as a reasoning tool
Types can help
- Planning code
- As entry checks for debugging
    - Check types of arguments on entry to ensure meet specifications
    - Check types of values returned to ensure meet specifications
    - Check constraints on values