# yaai_cloud - ABAP AI tools Cloud - Function Calling - Anthropic

The ABAP AI tools Cloud function calling feature integrates Anthropic's function calling capabilities with ABAP global classes, enabling large language models (LLMs) to invoke ABAP instance methods programmatically. This allows for dynamic, AI-driven workflows where LLMs can request the execution of ABAP logic and receive structured results.

## Overview

Anthropic function calling enables LLMs to interact with external functions by describing them in a machine-readable format. In the ABAP AI context, this means exposing ABAP class methods as callable functions. The ABAP AI framework serializes method signatures and parameters, allowing the LLM to understand available methods and their expected inputs.

Supported parameter types:
- Scalar types (e.g., `STRING`, `INT4`, `P`, `D`, `C`, ...)
- One-level structures (flat structures, no nested structures)
- One-level tables (internal tables with flat line types)

## Usage Steps

1. **Define ABAP Global Class and Method**  
    Create a global class with instance methods you want to expose. Ensure method parameters are scalar, flat structures, or flat tables. The method can only have `IMPORTING` parameters; `EXPORTING` and `CHANGING` parameters are not supported. The method must have a `RETURNING` parameter named `R_RESPONSE` of type `STRING`.

2. **Register the Class/Method**  
    Use the ABAP AI framework to register the class and method for function calling.  
    The method `add_methods` of the class `ycl_aaic_func_call_anthropic` must be used to register the methods you want to expose for function calling.  
    The framework will generate a function schema compatible with Anthropic.

    ```abap
    DATA(lo_function_calling) = NEW ycl_aaic_func_call_anthropic( ).

    lo_function_calling->add_methods( VALUE #( ( class_name = 'zcl_aaic_math_tools'
                                                 method_name = 'add'
                                                 description = 'Use this method to add two numbers (i_num1 + i_num2)' )

                                               ( class_name = 'zcl_aaic_math_tools'
                                                 method_name = 'subtract'
                                                 description = 'Use this method to subtract two numbers (i_num1 - i_num2)' )

                                               ( class_name = 'zcl_aaic_math_tools'
                                                 method_name = 'multiply'
                                                 description = 'Use this method to multiply two numbers (i_num1 * i_num2)' )

                                               ( class_name = 'zcl_aaic_math_tools'
                                                 method_name = 'divide'
                                                 description = 'Use this method to divide two numbers (i_num1 / i_num2)' ) ) ).
    ```

3. **Bind Tools to Anthropic**  
    The ABAP AI Anthropic `bind_tools` method expects an object (instance) of the class `ycl_aaic_func_call_anthropic` as its argument. This object manages the registration and invocation of ABAP methods as callable tools for Anthropic function calling.

    ```abap
    lo_aaic_anthropic->bind_tools( lo_function_calling ).
    ```

4. **Describe Functions to Anthropic**  
    The ABAP AI framework provides a JSON schema describing available methods and their parameters. This schema is sent to the Anthropic API as part of the function calling setup.

5. **Invoke via LLM**  
    When the LLM determines a function call is needed, it returns a function call request with parameter values. The ABAP AI framework parses this request and invokes the corresponding ABAP method.

6. **Return Result**  
    The ABAP method is executed, and its result is returned in the `R_RESPONSE` RETURNING parameter of type `STRING`. This value is then sent back to the LLM as the function call response.

**Complete Example:**

```abap
CLASS zcl_aaic_example_fc_anthropic DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_aaic_example_fc_anthropic IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    DATA: l_api_key             TYPE string,
          l_message             TYPE string,
          l_system_instructions TYPE string.

    DATA(lo_aaic_conn) = NEW ycl_aaic_conn( i_api = yif_aaic_const=>c_anthropic ).

    lo_aaic_conn->set_base_url( i_base_url = 'https://api.anthropic.com' ).

    lo_aaic_conn->set_api_key( i_api_key = l_api_key ).

    DATA(lo_aaic_anthropic) = NEW ycl_aaic_anthropic( i_model = 'claude-sonnet-4-20250514'
                                                      i_o_connection = lo_aaic_conn ).

    l_system_instructions = |# Identity\n|.
    l_system_instructions = |{ l_system_instructions }You are a friendly and helpful math support assistant.\n|.
    l_system_instructions = |{ l_system_instructions }Your primary role is to assist users with mathematical questions and problems.\n|.
    l_system_instructions = |{ l_system_instructions }You must use the provided calculation tools to solve numerical problems, ensuring accuracy.\n|.
    l_system_instructions = |{ l_system_instructions }Never do the calculations yourself. Always use the provided tools instead, respecting the results returned.\n|.
    l_system_instructions = |{ l_system_instructions }Call one tool at a time. You must wait for the result from one tool before making any subsequent tool calls.\n|.
    l_system_instructions = |{ l_system_instructions }Never send multiple tool calls at once.\n|.
    l_system_instructions = |{ l_system_instructions }Always be patient, encouraging, and explain your steps in a clear and understandable way.\n|.

    lo_aaic_anthropic->set_system_instructions( l_system_instructions ).

    DATA(lo_function_calling) = NEW ycl_aaic_func_call_anthropic( ).

    lo_function_calling->add_methods( VALUE #( ( class_name = 'zcl_aaic_math_tools'
                                                 method_name = 'add'
                                                 description = 'Use this method to add two numbers (i_num1 + i_num2)' )

                                               ( class_name = 'zcl_aaic_math_tools'
                                                 method_name = 'subtract'
                                                 description = 'Use this method to subtract two numbers (i_num1 - i_num2)' )

                                               ( class_name = 'zcl_aaic_math_tools'
                                                 method_name = 'multiply'
                                                 description = 'Use this method to multiply two numbers (i_num1 * i_num2)' )

                                               ( class_name = 'zcl_aaic_math_tools'
                                                 method_name = 'divide'
                                                 description = 'Use this method to divide two numbers (i_num1 / i_num2)' ) ) ).

    lo_aaic_anthropic->bind_tools( lo_function_calling ).

    l_message = 'Hi there! What is the result of 5 + 7?'.

    lo_aaic_anthropic->chat(
      EXPORTING
        i_message    = l_message
      IMPORTING
        e_t_response = DATA(lt_response)
    ).

    out->write( lt_response ).

  ENDMETHOD.

ENDCLASS.
```

## Limitations

- Only instance methods of global classes are supported.
- Methods can only have `IMPORTING` parameters; `EXPORTING` and `CHANGING` parameters are not supported.
- Each method must have a `RETURNING` parameter named `R_RESPONSE` of type `STRING`.
- Method parameters must be scalar, one-level structures, or one-level tables.
- Nested structures and tables are not supported.
- Complex data types (e.g., objects, deep structures) are not supported.

## Function Calling with Proxy Class Support

### Overview

ABAP AI Tools supports the execution of ABAP logic via Large Language Models (LLMs) by allowing methods to be called dynamically. However, because ABAP is a strongly typed language, the parameters expected by ABAP methods must be in precise formats and types.

LLMs, on the other hand, often struggle to provide perfectly formatted values. To bridge this gap and avoid runtime errors (e.g. type conversion errors, short dumps), we introduce the Proxy Class Pattern.

### Problem

When LLMs call ABAP tools, they must provide parameter values that exactly match the expected ABAP data types defined in the method signatures. For instance:

```abap
METHODS calculate_total
  IMPORTING
    value1 TYPE f
    value2 TYPE f
  RETURNING VALUE(r_response) TYPE string.
```

A minor mismatch in types or formats (e.g., a string `"10.00"` instead of a number) can lead to a short dump.

### Solution: Proxy Class Interface

To prevent type mismatch issues, ABAP AI Tools allows you to define a Proxy Class alongside your concrete implementation.

 - The **Concrete Class** is used to generate the **JSON Schema** that will be sent to the LLM, ensuring it understands the method signature and data format.

 - The **Proxy Class** is used to receive and validate/convert inputs coming from the LLM, before calling the concrete method.
 
 The developer handles type conversion, error handling, and validation inside the proxy method.

 **Example**:

1. Concrete Class (Strict Types)

```abap
CLASS zcl_calculator DEFINITION.
  
  PUBLIC SECTION.
  
    METHODS add
      IMPORTING
        i_num1   TYPE f
        i_num2   TYPE f
      RETURNING VALUE(r_response) TYPE string.

ENDCLASS.

CLASS zcl_calculator IMPLEMENTATION.

  METHOD add.

    r_response = |The result of { i_num1 } + { i_num2 } is { i_num1 + i_num2 }|.

  ENDMETHOD.

ENDCLASS.
```

2. Proxy Class (Flexible Types)

```abap
CLASS zcl_calculator_proxy DEFINITION.
  
  PUBLIC SECTION.
    
    METHODS add
      IMPORTING 
        i_num1 TYPE string
        i_num1 TYPE string
      RETURNING VALUE(r_response) TYPE string.

ENDCLASS.

CLASS zcl_calculator_proxy IMPLEMENTATION.

  METHOD add.

    DATA: l_num1 TYPE f,
          l_num2 TYPE f.

    "Handle type convertions and/or execute validations before calling the concrete method
    TRY.

        l_num1 = i_num1.
        l_num2 = i_num2.

      CATCH cx_sy_conversion_no_number.
        r_response = 'Error: Invalid number format'.
        RETURN.
    ENDTRY.

    "Call the concrete method
    r_response = NEW zcl_aaic_math_tools( )->add(
      EXPORTING
        i_num1 = l_num1
        i_num2 = l_num2
    ).

  ENDMETHOD.

ENDCLASS.
```
### How It Works Internally

1. Schema Generation: ABAP AI Tools uses **RTTI (Runtime Type Information)** to inspect the concrete class method signature and generate a JSON schema.

2. Execution: When the LLM returns a tool call, ABAP AI Tools:
  - Calls the proxy class method with the LLM-provided input values.
  - Inside the proxy, the values are validated and/or converted.
  - Finally, the concrete class logic is invoked with the correctly typed parameters.

### Using the Proxy Class

To use a proxy class for function calling, simply specify its name in the `proxy_class` parameter when invoking the `add_methods` method.

**Example**:

```abap
DATA(lo_function_calling) = NEW ycl_aaic_func_call_anthropic( ).

lo_function_calling->add_methods( VALUE #( ( class_name = 'zcl_aaic_math_tools_proxy'
                                             method_name = 'add'
                                             description = 'Use this method to add two numbers (i_num1 + i_num2)' ) ) ).
```