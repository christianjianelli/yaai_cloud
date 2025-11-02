INTERFACE yif_aaic_agent_db
  PUBLIC.

  TYPES ty_agent_tools_t TYPE STANDARD TABLE OF yaaic_agent_tool WITH DEFAULT KEY.

  METHODS create
    IMPORTING
      i_s_agent       TYPE yaaic_agent
      i_t_agent_tools TYPE ty_agent_tools_t
    EXPORTING
      e_id            TYPE uuid
      e_error         TYPE string.

  METHODS read
    IMPORTING
      i_agent_id      TYPE yaaic_agent-id OPTIONAL
      i_agent_name    TYPE yaaic_agent-name OPTIONAL
    EXPORTING
      e_s_agent       TYPE yaaic_agent
      e_t_agent_tools TYPE ty_agent_tools_t
      e_error         TYPE string.

  METHODS update
    IMPORTING
      i_s_agent       TYPE yaaic_agent
      i_t_agent_tools TYPE ty_agent_tools_t
    EXPORTING
      e_updated       TYPE abap_bool
      e_error         TYPE string.

  METHODS delete
    IMPORTING
      i_agent_id   TYPE yaaic_agent-id OPTIONAL
    EXPORTING
      e_deleted    TYPE abap_bool
      e_error      TYPE string.

ENDINTERFACE.
