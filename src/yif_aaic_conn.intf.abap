INTERFACE yif_aaic_conn
  PUBLIC .

  TYPES: BEGIN OF ty_http_header_s,
           name  TYPE string,
           value TYPE string,
         END OF ty_http_header_s.

  DATA: mo_log     TYPE REF TO ycl_aaic_log READ-ONLY,
        mo_api_key TYPE REF TO yif_aaic_api_key READ-ONLY.

  DATA: mt_msg         TYPE yif_aaic_log=>ty_messages_t READ-ONLY,
        mt_http_header TYPE STANDARD TABLE OF ty_http_header_s READ-ONLY.

  DATA: m_api                   TYPE string READ-ONLY,
        m_base_url              TYPE string READ-ONLY,
        m_endpoint              TYPE string READ-ONLY.

  EVENTS on_request_send.
  EVENTS on_response_received.
  EVENTS on_connection_error.

  METHODS
    create
      IMPORTING
                i_endpoint       TYPE string
                i_api_key        TYPE string OPTIONAL
                i_body_json      TYPE string OPTIONAL
      RETURNING VALUE(r_created) TYPE abap_bool.

  METHODS
    set_base_url
      IMPORTING
        i_base_url TYPE string.

  METHODS
    set_api_key
      IMPORTING
        i_o_api_key TYPE REF TO yif_aaic_api_key OPTIONAL
        i_api_key   TYPE string OPTIONAL.

  METHODS
    set_body
      IMPORTING
        i_json TYPE string.

  METHODS
    execute
      EXPORTING
        e_response TYPE string
        e_failed   TYPE abap_bool.

  METHODS
    get_last_response
      RETURNING VALUE(r_response) TYPE REF TO if_web_http_response.

  METHODS
    get_error_text
      EXPORTING
        e_error_text TYPE string.

  METHODS add_http_header_param
    IMPORTING
      i_name  TYPE csequence
      i_value TYPE csequence.

  METHODS remove_http_header_param
    IMPORTING
      i_name TYPE csequence.


ENDINTERFACE.
