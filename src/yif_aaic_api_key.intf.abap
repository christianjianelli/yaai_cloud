INTERFACE yif_aaic_api_key
  PUBLIC.

  METHODS get
    IMPORTING
              i_id             TYPE clike
    RETURNING VALUE(r_api_key) TYPE string.

ENDINTERFACE.
