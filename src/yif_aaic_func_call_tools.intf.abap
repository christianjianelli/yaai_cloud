INTERFACE yif_aaic_func_call_tools
  PUBLIC.

  METHODS get_list
    IMPORTING
              i_chat_id         TYPE yaaic_chat-id OPTIONAL
              i_agent_id        TYPE yaaic_agent-id OPTIONAL
    RETURNING VALUE(r_response) TYPE string.

ENDINTERFACE.
