INTERFACE yif_aaic_rag_db
  PUBLIC.

  METHODS create
    IMPORTING
      i_filename    TYPE csequence
      i_description TYPE csequence OPTIONAL
      i_keywords    TYPE csequence OPTIONAL
      i_content     TYPE string
    EXPORTING
      e_id          TYPE uuid.

  METHODS read
    IMPORTING
      i_id          TYPE uuid
    EXPORTING
      e_filename    TYPE csequence
      e_description TYPE csequence
      e_keywords    TYPE csequence
      e_content     TYPE string.

  METHODS update
    IMPORTING
      i_id          TYPE uuid
      i_description TYPE csequence OPTIONAL
      i_keywords    TYPE csequence OPTIONAL
    EXPORTING
      e_updated     TYPE abap_bool.

  METHODS delete
    IMPORTING
      i_id      TYPE uuid
    EXPORTING
      e_deleted TYPE abap_bool.

ENDINTERFACE.
