INTERFACE yif_aaic_chat
  PUBLIC.

  TYPES: ty_response_t TYPE STANDARD TABLE OF string.

  EVENTS on_message_send.
  EVENTS on_response_received.
  EVENTS on_message_failed.

  METHODS chat
    IMPORTING
      id           TYPE uuid OPTIONAL
      i_message    TYPE csequence
      i_new        TYPE abap_bool DEFAULT abap_false
      i_greeting   TYPE csequence OPTIONAL
    EXPORTING
      e_id         TYPE uuid
      e_response   TYPE string
      e_failed     TYPE abap_bool
      e_t_response TYPE ty_response_t.

ENDINTERFACE.
