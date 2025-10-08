# yaai_cloud - ABAP AI tools Cloud - System Instructions - Google

## Overview

System instructions (also known as "system prompts") are special messages provided to a Large Language Model (LLM) to guide its behavior, tone, or constraints during a conversation. In ABAP AI, you can pass system instructions to the LLM using the `set_system_instructions` method of the `ycl_aaic_google` class.

## Purpose

System instructions are typically used to:

- Define the assistant's persona (e.g., "You are a helpful SAP consultant.")
- Set boundaries or rules for responses
- Specify the format or style of answers
- Provide context or background information

## How to Use

1. **Create the Connection and the LLM Client Instances**

   ```abap
    DATA(lo_aaic_conn) = NEW ycl_aaic_conn( i_api = yif_aaic_const=>c_google ).

    lo_aaic_conn->set_base_url( i_base_url = 'https://generativelanguage.googleapis.com' ).

    lo_aaic_conn->set_api_key( i_api_key = l_api_key ).

    DATA(lo_aaic_google) = NEW ycl_aaic_google( i_model = 'gemini-2.5-flash'
                                                i_o_connection = lo_aaic_conn ).
   ```

2. **Set the System Instructions**

   Call the `set_system_instructions` method, passing your instructions as a string.

   ```abap
    DATA l_system_instructions TYPE string.

    l_system_instructions = |# Identity\n|.
    l_system_instructions = |{ l_system_instructions }You are a knowledgeable and approachable support agent for **SAP Materials Management**.\n|.

    lo_aaic_google->set_system_instructions( l_system_instructions ).
   ```

3. **Continue with Message Generation**

   After setting the system instructions, you can proceed to generate messages as usual. The instructions will be included in the conversation context sent to the LLM.

   ```abap
    l_message = 'Hi there! What is your area of expertise?'.

    lo_aaic_google->chat(
      EXPORTING
        i_message    = l_message
      IMPORTING
        e_t_response = DATA(lt_response)
    ).
   ```

## Complete Example

```abap
CLASS zcl_aaic_example_si_google DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_aaic_example_si_google IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    DATA: l_api_key             TYPE string,
          l_message             TYPE string,
          l_system_instructions TYPE string.

    DATA(lo_aaic_conn) = NEW ycl_aaic_conn( i_api = yif_aaic_const=>c_google ).

    lo_aaic_conn->set_base_url( i_base_url = 'https://generativelanguage.googleapis.com' ).

    lo_aaic_conn->set_api_key( i_api_key = l_api_key ).

    DATA(lo_aaic_google) = NEW ycl_aaic_google( i_model = 'gemini-2.5-flash'
                                                i_o_connection = lo_aaic_conn ).

    l_system_instructions = |# Identity\n|.
    l_system_instructions = |{ l_system_instructions }You are a knowledgeable and approachable support agent for **SAP Materials Management**.\n|.

    lo_aaic_google->set_system_instructions( l_system_instructions ).

    l_message = 'Hi there! What is your area of expertise?'.

    lo_aaic_google->chat(
      EXPORTING
        i_message    = l_message
      IMPORTING
        e_t_response = DATA(lt_response)
    ).

    out->write( lt_response ).

  ENDMETHOD.

ENDCLASS.
```