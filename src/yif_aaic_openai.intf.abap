INTERFACE yif_aaic_openai
  PUBLIC.

  TYPES: ty_response_t TYPE STANDARD TABLE OF string.

  TYPES: BEGIN OF ty_generate_message_s,
           role      TYPE string,
           content   TYPE string,
           type      TYPE string,
           call_id   TYPE string,
           arguments TYPE string,
           name      TYPE string,
           output    TYPE string,
         END OF ty_generate_message_s.

  TYPES: ty_generate_messages_t TYPE STANDARD TABLE OF ty_generate_message_s WITH EMPTY KEY.

  TYPES: BEGIN OF ty_type_message_s,
           role    TYPE string,
           content TYPE string,
           type    TYPE string,
         END OF ty_type_message_s,

         BEGIN OF ty_type_text_s,
           verbosity TYPE string,
         END OF ty_type_text_s,

         BEGIN OF ty_type_reasoning_s,
           effort TYPE string,
         END OF ty_type_reasoning_s,

         BEGIN OF ty_error_s,
           code    TYPE string,
           message TYPE string,
         END OF ty_error_s,

         BEGIN OF ty_function_call_s,
           type      TYPE string,
           arguments TYPE string,
           call_id   TYPE string,
           name      TYPE string,
         END OF ty_function_call_s,

         BEGIN OF ty_function_call_output_s,
           type    TYPE string,
           call_id TYPE string,
           output  TYPE string,
         END OF ty_function_call_output_s,

         BEGIN OF ty_function_s,
           name      TYPE string,
           arguments TYPE string,
         END OF ty_function_s,

         BEGIN OF ty_function_call_chat_comp_s,
           id       TYPE string,
           type     TYPE string,
           function TYPE ty_function_s,
         END OF ty_function_call_chat_comp_s,

         ty_function_call_chat_comp_t TYPE STANDARD TABLE OF ty_function_call_chat_comp_s WITH EMPTY KEY,

         BEGIN OF ty_type_message_chat_comp_nt_s, " Message
           role    TYPE string,
           content TYPE string,
         END OF ty_type_message_chat_comp_nt_s,

         BEGIN OF ty_type_message_chat_comp_tc_s, " Function/Tool Calling Message
           role       TYPE string,
           tool_calls TYPE ty_function_call_chat_comp_t,
         END OF ty_type_message_chat_comp_tc_s,

         BEGIN OF ty_type_message_chat_comp_tr_s, " Function/Tool Response Message
           role         TYPE string,
           content      TYPE string,
           tool_call_id TYPE string,
         END OF ty_type_message_chat_comp_tr_s,

         BEGIN OF ty_type_message_chat_comp_s,
           role       TYPE string,
           content    TYPE string,
           tool_calls TYPE ty_function_call_chat_comp_t,
         END OF ty_type_message_chat_comp_s.

  TYPES: BEGIN OF ty_openai_generate_request_s,
           model       TYPE string,
           stream      TYPE abap_bool,
           input       TYPE /ui2/cl_json=>json,
           text        TYPE ty_type_text_s,
           reasoning   TYPE ty_type_reasoning_s,
           tools       TYPE /ui2/cl_json=>json,
         END OF ty_openai_generate_request_s.

  TYPES: BEGIN OF ty_openai_generate_req_wt_s,
           model       TYPE string,
           stream      TYPE abap_bool,
           temperature TYPE p LENGTH 2 DECIMALS 1,
           input       TYPE /ui2/cl_json=>json,
           tools       TYPE /ui2/cl_json=>json,
         END OF ty_openai_generate_req_wt_s.

  TYPES: BEGIN OF ty_openai_completions_req_s,
           model       TYPE string,
           stream      TYPE abap_bool,
           temperature TYPE p LENGTH 2 DECIMALS 1,
           messages    TYPE /ui2/cl_json=>json,
         END OF ty_openai_completions_req_s.

  TYPES: BEGIN OF ty_openai_comp_tools_req_s,
           model       TYPE string,
           stream      TYPE abap_bool,
           temperature TYPE p LENGTH 2 DECIMALS 1,
           messages    TYPE /ui2/cl_json=>json,
           tools       TYPE /ui2/cl_json=>json,
         END OF ty_openai_comp_tools_req_s.

  TYPES: BEGIN OF ty_content_s,
           type TYPE string,
           text TYPE string,
         END OF ty_content_s.

  TYPES content_t TYPE STANDARD TABLE OF ty_content_s WITH NON-UNIQUE KEY text.

  TYPES: BEGIN OF ty_output_s,
           id        TYPE string,
           type      TYPE string,
           role      TYPE string,
           content   TYPE content_t,
           call_id   TYPE string,
           status    TYPE string,
           name      TYPE string,
           arguments TYPE /ui2/cl_json=>json,
           output    TYPE string,
         END OF ty_output_s.

  TYPES: ty_output_t TYPE STANDARD TABLE OF ty_output_s WITH NON-UNIQUE KEY id.

  TYPES: BEGIN OF ty_choices_s,
           index   TYPE i,
           message TYPE ty_type_message_chat_comp_s,
         END OF ty_choices_s.

  TYPES: ty_choices_t TYPE STANDARD TABLE OF ty_choices_s WITH NON-UNIQUE KEY index.

  TYPES: BEGIN OF ty_openai_generate_response_s,
           id          TYPE string,
           status      TYPE string,
           model       TYPE string,
           temperature TYPE string,
           error       TYPE ty_error_s,
           output      TYPE ty_output_t,
         END OF ty_openai_generate_response_s.

  TYPES: BEGIN OF ty_openai_chat_comp_resp_s,
           id      TYPE string,
           object  TYPE string,
           model   TYPE string,
           error   TYPE ty_error_s,
           choices TYPE ty_choices_t,
         END OF ty_openai_chat_comp_resp_s.

  TYPES: BEGIN OF ty_openai_embed_request_s,
           model TYPE string,
           input TYPE string,
         END OF ty_openai_embed_request_s.

  TYPES: ty_embedding_t TYPE STANDARD TABLE OF f WITH DEFAULT KEY.

  TYPES: BEGIN OF ty_openai_embed_data_s,
           object    TYPE string,
           embedding TYPE ty_embedding_t,
           index     TYPE i,
         END OF ty_openai_embed_data_s.

  TYPES: ty_openai_embed_data_t TYPE STANDARD TABLE OF ty_openai_embed_data_s WITH DEFAULT KEY.

  TYPES: BEGIN OF ty_openai_embed_response_s,
           object TYPE string,
           data   TYPE ty_openai_embed_data_t,
           model  TYPE string,
         END OF ty_openai_embed_response_s.

  CONSTANTS: mc_verbosity_low            TYPE string VALUE 'low',
             mc_verbosity_medium         TYPE string VALUE 'medium',
             mc_verbosity_high           TYPE string VALUE 'high',
             mc_reasoning_effort_minimal TYPE string VALUE 'minimal',
             mc_reasoning_effort_low     TYPE string VALUE 'low',
             mc_reasoning_effort_medium  TYPE string VALUE 'medium',
             mc_reasoning_effort_high    TYPE string VALUE 'high'.

  DATA: mo_function_calling TYPE REF TO yif_aaic_func_call_openai READ-ONLY.

  METHODS use_completions
    IMPORTING
      i_use_completions TYPE abap_bool DEFAULT abap_true.

  METHODS set_model
    IMPORTING
      i_model TYPE csequence.

  METHODS set_temperature
    IMPORTING
      i_temperature TYPE numeric.

  METHODS set_verbosity
    IMPORTING
      i_verbosity TYPE csequence.

  METHODS set_reasoning_effort
    IMPORTING
      i_reasoning_effort TYPE csequence.

  METHODS set_system_instructions
    IMPORTING
      i_system_instructions      TYPE string
      i_system_instructions_role TYPE string DEFAULT 'developer'.

  METHODS set_connection
    IMPORTING
      i_o_connection TYPE REF TO yif_aaic_conn.

  METHODS set_persistence
    IMPORTING
      i_o_persistence TYPE REF TO yif_aaic_db.

  METHODS bind_tools
    IMPORTING
      i_o_function_calling TYPE REF TO yif_aaic_func_call_openai
      i_max_tools_calls    TYPE i DEFAULT 5.

  METHODS set_history
    IMPORTING
      i_t_history TYPE ty_generate_messages_t.

  METHODS get_history
    EXPORTING
      e_t_history TYPE ty_generate_messages_t.

  METHODS get_conversation
    RETURNING
      VALUE(r_conversation) TYPE /ui2/cl_json=>json.

  METHODS get_conversation_chat_comp
    RETURNING
      VALUE(r_conversation) TYPE /ui2/cl_json=>json.

  METHODS generate
    IMPORTING
      i_message    TYPE csequence
      i_new        TYPE abap_bool DEFAULT abap_false
      i_greeting   TYPE csequence OPTIONAL
    EXPORTING
      e_response   TYPE string
      e_failed     TYPE abap_bool
      e_t_response TYPE ty_response_t.

  METHODS chat_completions
    IMPORTING
      i_message    TYPE csequence
      i_new        TYPE abap_bool DEFAULT abap_false
      i_greeting   TYPE csequence OPTIONAL
    EXPORTING
      e_response   TYPE string
      e_failed     TYPE abap_bool
      e_t_response TYPE ty_response_t.

  METHODS embed
    IMPORTING
      i_input      TYPE csequence
    EXPORTING
      e_s_response TYPE ty_openai_embed_response_s.

ENDINTERFACE.
