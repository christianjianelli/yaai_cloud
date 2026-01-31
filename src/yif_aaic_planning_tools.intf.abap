INTERFACE yif_aaic_planning_tools
  PUBLIC.

  METHODS create_plan
    IMPORTING
              i_plan            TYPE string
    RETURNING VALUE(r_response) TYPE yde_aaic_response.

  METHODS get_plan
    RETURNING VALUE(r_response) TYPE yde_aaic_response.

  METHODS update_plan
    IMPORTING
              i_plan            TYPE string
              i_append          TYPE abap_bool DEFAULT abap_true
    RETURNING VALUE(r_response) TYPE yde_aaic_response.

  METHODS delete_plan
    RETURNING VALUE(r_response) TYPE yde_aaic_response.

ENDINTERFACE.
