# yaai_cloud - ABAP AI tools Cloud - Anthropic

<p>
  <img src="../images/anthropic_logo.svg" alt="Anthropic Logo" width="200px">
</p>

## Anthropic Quickstart

### Running Your First ABAP AI Cloud Anthropic Application

This quickstart demonstrates how to create a simple LLM application. It shows you how to connect to the LLM and perform a basic chat interaction.

**Requirements:** 
*   You have a valid Anthropic API Key.

**Steps:**
1.  Create an ABAP AI Connection instance;
2.  Set the Base URL;
3.  Set the API Key;
4.  Create an ABAP AI Anthropic instance;
5.  Call the CHAT method.

**Example:**
```abap
CLASS zcl_aaic_example_anthropic DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_aaic_example_anthropic IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    DATA: l_api_key TYPE string,
          l_message TYPE string.

    DATA(lo_aaic_conn) = NEW ycl_aaic_conn( i_api = yif_aaic_const=>c_anthropic ).

    lo_aaic_conn->set_base_url( i_base_url = 'https://api.anthropic.com' ).

    lo_aaic_conn->set_api_key( i_api_key = l_api_key ).

    DATA(lo_aaic_anthropic) = NEW ycl_aaic_anthropic( i_model = 'claude-sonnet-4-20250514'
                                                      i_o_connection = lo_aaic_conn ).

    l_message = 'Hi there! What is the capital of France?'.

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

**How to run:**
1. Set a breakpoint on the `set_api_key` method call (`lo_aaic_conn->set_api_key( i_api_key = l_api_key ).`).
2. Press F9 to execute the code.
3. In the debugger, set the value of the variable `l_api_key` to your actual API key.
4. Press F8 to continue execution.

**ABAP Debugger:**

![Output of the ABAP AI quickstart example](../images/quickstart_anthropic_1.png)

**Result (ABAP Console view):**

![Output of the ABAP AI quickstart example](../images/quickstart_anthropic_2.png)