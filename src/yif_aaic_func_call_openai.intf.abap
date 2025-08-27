INTERFACE yif_aaic_func_call_openai
  PUBLIC .

  TYPES: BEGIN OF ty_parameters_s,
           type       TYPE string,
           properties TYPE /ui2/cl_json=>json,
           required   TYPE STANDARD TABLE OF string WITH EMPTY KEY,
         END OF ty_parameters_s,

         BEGIN OF ty_tool_s,
           type        TYPE string,
           name        TYPE string,
           description TYPE string,
           parameters  TYPE ty_parameters_s,
         END OF ty_tool_s,

         ty_tools_t TYPE STANDARD TABLE OF ty_tool_s WITH EMPTY KEY,

         BEGIN OF ty_function_s,
           name        TYPE string,
           description TYPE string,
           parameters  TYPE ty_parameters_s,
         END OF ty_function_s,

         BEGIN OF ty_tool_chat_completion_s,
           type     TYPE string,
           function TYPE ty_function_s,
         END OF ty_tool_chat_completion_s,

         ty_tools_chat_completion_t TYPE STANDARD TABLE OF ty_tool_chat_completion_s WITH EMPTY KEY,

         BEGIN OF ty_method_s,
           class_name  TYPE string,
           proxy_class TYPE string,
           method_name TYPE string,
           description TYPE string,
           full_schema TYPE abap_bool,
         END OF ty_method_s,

         ty_methods_t TYPE STANDARD TABLE OF ty_method_s WITH EMPTY KEY.

  EVENTS on_tool_call
    EXPORTING
      VALUE(class_name)       TYPE string
      VALUE(method_name)      TYPE string
      VALUE(parameters_table) TYPE abap_parmbind_tab.

  EVENTS on_tool_call_response
    EXPORTING
      VALUE(tool_response) TYPE string.

  EVENTS on_tool_call_error
    EXPORTING
      VALUE(error_text) TYPE string.

  DATA: mt_methods TYPE ty_methods_t READ-ONLY.

  METHODS add_methods IMPORTING i_t_methods TYPE ty_methods_t.

  METHODS get_tools EXPORTING e_tools TYPE string.

  METHODS get_tools_chat_completions EXPORTING e_tools TYPE string.

  METHODS reset_methods.

  METHODS remove_method IMPORTING i_s_method TYPE ty_method_s.

  METHODS call_tool
    IMPORTING
              i_tool_name       TYPE string
              i_json            TYPE /ui2/cl_json=>json
    RETURNING VALUE(r_response) TYPE string.

ENDINTERFACE.
