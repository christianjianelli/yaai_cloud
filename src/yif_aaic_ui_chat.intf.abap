INTERFACE yif_aaic_ui_chat
  PUBLIC.

  METHODS get_html
    IMPORTING
              i_api         TYPE string OPTIONAL
              i_chat_id     TYPE string OPTIONAL
    RETURNING VALUE(r_html) TYPE string.

  METHODS get_css
    RETURNING VALUE(r_css) TYPE string.

ENDINTERFACE.
