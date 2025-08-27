INTERFACE yif_aaic_log
  PUBLIC .

  TYPES ty_messages_t TYPE STANDARD TABLE OF bapiret2 WITH DEFAULT KEY.

  DATA: mt_messages TYPE ty_messages_t.

  METHODS add_message
    IMPORTING
      i_s_msg TYPE bapiret2.

  METHODS add_messages
    IMPORTING
      i_t_msg TYPE ty_messages_t.

ENDINTERFACE.
