INTERFACE yif_aaic_agent
  PUBLIC.

  TYPES: BEGIN OF ty_agent_tool_s,
           class_name  TYPE yaaic_agent_tool-class_name,
           method_name TYPE yaaic_agent_tool-method_name,
           proxy_class TYPE yaaic_agent_tool-proxy_class,
           description TYPE yaaic_agent_tool-description,
         END OF ty_agent_tool_s,

         ty_agent_tools_t TYPE STANDARD TABLE OF ty_agent_tool_s WITH DEFAULT KEY.

  DATA: m_agent_id TYPE yaaic_agent-id READ-ONLY.

  METHODS get_system_instructions
    IMPORTING
              i_agent_id           TYPE yaaic_agent-id OPTIONAL
              i_agent_name         TYPE yaaic_agent-name OPTIONAL
    RETURNING VALUE(r_sys_inst_id) TYPE yaaic_agent-sys_inst_id.

  METHODS get_rag_data
    IMPORTING
              i_agent_id          TYPE yaaic_agent-id OPTIONAL
              i_agent_name        TYPE yaaic_agent-name OPTIONAL
    RETURNING VALUE(r_rag_ctx_id) TYPE yaaic_agent-rag_ctx_id.

  METHODS get_tools
    IMPORTING
              i_agent_id             TYPE yaaic_agent-id OPTIONAL
              i_agent_name           TYPE yaaic_agent-name OPTIONAL
    RETURNING VALUE(r_t_agent_tools) TYPE ty_agent_tools_t.

ENDINTERFACE.
