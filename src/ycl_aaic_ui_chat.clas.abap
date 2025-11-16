CLASS ycl_aaic_ui_chat DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_aaic_ui_chat.

    ALIASES get_html FOR yif_aaic_ui_chat~get_html.
    ALIASES get_css  FOR yif_aaic_ui_chat~get_css.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS YCL_AAIC_UI_CHAT IMPLEMENTATION.


  METHOD yif_aaic_ui_chat~get_css.

    r_css = '/* General Body and Font Styles */' && cl_abap_char_utilities=>newline.
    r_css = r_css && 'body {' && cl_abap_char_utilities=>newline.
    r_css = r_css && 'font-family: ''Segoe UI'', Tahoma, Geneva, Verdana, sans-serif;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '  /* A system font that looks native on Windows */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '  margin: 0;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '  padding: 10px;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '  /* Overall padding around the message area */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '  background-color: #f0f2f5;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '  /* Very light grey background for a clean base */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '  color: #333;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '  /* Dark grey for general text for good contrast */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '  line-height: 1.45;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '  /* Enhances readability by adding space between lines */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '  font-size: 0.9em;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '  /* Slightly reduced base font size to fit more content in the narrow window */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '  box-sizing: border-box;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '  /* Crucial: Includes padding and border in element''s total width/height */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '  overflow-y: auto;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '  /* Enable vertical scrolling if content overflows */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '  height: 100vh;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '  /* Ensure the body takes full viewport height for proper scrolling */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '}' && cl_abap_char_utilities=>newline.

    r_css = r_css && cl_abap_char_utilities=>newline.

    r_css = r_css && '/* Message Container: Flexbox for vertical stacking */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '.message-container {' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    align-items: center;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    justify-content: flex-start;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    margin: 0 auto;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    width: 50%;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    display: flex;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    flex-direction: column;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    gap: 15px;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    /* Spacing between individual messages */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    max-width: 100%;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    /* Ensures the container doesn''t overflow its parent */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '}' && cl_abap_char_utilities=>newline.

    r_css = r_css && cl_abap_char_utilities=>newline.

    r_css = r_css && '/* Individual Message Styles */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '.message {' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    display: flex;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    flex-direction: column;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    max-width: 100%;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    /* Each message takes full available width */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    /* Default alignment for LLM messages (align-items: flex-start) */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '}' && cl_abap_char_utilities=>newline.

    r_css = r_css && cl_abap_char_utilities=>newline.

    r_css = r_css && '/* Message Bubble Styling */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '.message-bubble {' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    padding: 10px 14px;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    border-radius: 18px;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    /* Smoothly rounded corners for a modern chat look */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    max-width: calc(100% - 40px);' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    /* Limits bubble width, leaving space on the opposite side */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    word-wrap: break-word;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    /* Prevents long words/URLs from overflowing */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    box-shadow: 0 1px 2px rgba(0, 0, 0, 0.08);' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    /* Subtle shadow for depth */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    position: relative;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    /* Needed for any future additions like small "tail" elements */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    min-width: 300px;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '}' && cl_abap_char_utilities=>newline.

    r_css = r_css && cl_abap_char_utilities=>newline.

    r_css = r_css && '.message-bubble p {' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    margin: 0;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    /* Remove default paragraph margins inside the bubble */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '}' && cl_abap_char_utilities=>newline.

    r_css = r_css && cl_abap_char_utilities=>newline.

    r_css = r_css && '/* LLM (AI Assistant) Message Specific Styles */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '.llm-message {' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    align-self: flex-start;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    align-items: flex-start;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    /* Aligns LLM messages to the left */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '}' && cl_abap_char_utilities=>newline.

    r_css = r_css && cl_abap_char_utilities=>newline.

    r_css = r_css && '.llm-message .message-bubble {' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    background-color: #ffffff;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    /* Very light blue for LLM messages */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    color: #2c3e50;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    /* Darker blue-grey for LLM text */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    border-bottom-left-radius: 4px;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    /* Slightly less rounded corner for a distinct visual cue */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '}' && cl_abap_char_utilities=>newline.

    r_css = r_css && cl_abap_char_utilities=>newline.

    r_css = r_css && '/* User Message Specific Styles */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '.user-message {' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    align-self: flex-end;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    /* Pushes the entire message block to the right */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    align-items: flex-end;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    /* Aligns content (bubble, timestamp) within the block to the right */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '}' && cl_abap_char_utilities=>newline.

    r_css = r_css && cl_abap_char_utilities=>newline.

    r_css = r_css && '.user-message .message-bubble {' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    background-color: #e9eef6;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    /* Soft green for user messages */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    color: #2c3e50;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    /* Same dark blue-grey for user text */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    border-bottom-right-radius: 4px;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    /* Slightly less rounded corner for a distinct visual cue */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '}' && cl_abap_char_utilities=>newline.

    r_css = r_css && cl_abap_char_utilities=>newline.

    r_css = r_css && '/* Message Timestamp */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '.llm-typing-dot {' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    width: 7px;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    height: 7px;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    margin: 0 2px;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    border-radius: 50%;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    background: #a0a0a0;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    opacity: 0.5;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    animation: llmTypingBlink 1.2s infinite both;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '}' && cl_abap_char_utilities=>newline.

    r_css = r_css && cl_abap_char_utilities=>newline.

    r_css = r_css && '.llm-typing-dot:nth-child(2) {' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    animation-delay: 0.2s;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '}' && cl_abap_char_utilities=>newline.

    r_css = r_css && cl_abap_char_utilities=>newline.

    r_css = r_css && '.llm-typing-dot:nth-child(3) {' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    animation-delay: 0.4s;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '}' && cl_abap_char_utilities=>newline.

    r_css = r_css && cl_abap_char_utilities=>newline.

    r_css = r_css && '@keyframes llmTypingBlink {' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    0%, 80%, 100% { opacity: 0.5; }' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    40% { opacity: 1; }' && cl_abap_char_utilities=>newline.
    r_css = r_css && '}' && cl_abap_char_utilities=>newline.

    r_css = r_css && cl_abap_char_utilities=>newline.

    r_css = r_css && 'pre {' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    background: #fff;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    border-left: 3px solid #c9d5e9;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    border-radius: 0 4px 4px 0;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    padding: 16px;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    color: #333;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    font-family: ''Courier New'', Courier, monospace;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    font-size: 12px;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    line-height: 1.4;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    overflow-x: auto;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    margin: 1em 0;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    box-shadow: 0 1px 3px rgba(0,0,0,0.12);' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    scrollbar-width: thin;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '}' && cl_abap_char_utilities=>newline.

    r_css = r_css && cl_abap_char_utilities=>newline.

    r_css = r_css && '/* For WebKit browsers (Chrome, Safari) */' && cl_abap_char_utilities=>newline.
    r_css = r_css && 'pre::-webkit-scrollbar {' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    height: 5px;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    width: 5px;' && cl_abap_char_utilities=>newline.

    r_css = r_css && '}' && cl_abap_char_utilities=>newline.

    r_css = r_css && '.message-timestamp {' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    font-size: 0.7em;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    /* Smaller font size for the timestamp */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    color: #888;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    /* Muted grey for timestamps */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    margin-top: 4px;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    /* Space between bubble and timestamp */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    padding: 0 8px;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    /* Horizontal padding to align with bubble content */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '}' && cl_abap_char_utilities=>newline.

    r_css = r_css && cl_abap_char_utilities=>newline.

    r_css = r_css && '.user-message .message-timestamp {' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    text-align: right;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    /* Aligns user timestamps to the right */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '}' && cl_abap_char_utilities=>newline.

    r_css = r_css && cl_abap_char_utilities=>newline.

    r_css = r_css && '.llm-message .message-timestamp {' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    text-align: left;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    /* Aligns LLM timestamps to the left */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '}' && cl_abap_char_utilities=>newline.

    r_css = r_css && cl_abap_char_utilities=>newline.

    r_css = r_css && '/* User Typing Animation */' && cl_abap_char_utilities=>newline.
    r_css = r_css && '.llm-typing {' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    display: flex;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    align-items: center;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    height: 24px;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    margin-right: 8px;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '}' && cl_abap_char_utilities=>newline.
    r_css = r_css && cl_abap_char_utilities=>newline.
    r_css = r_css && '.form-container {' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    margin-top: 50px;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '}' && cl_abap_char_utilities=>newline.

    r_css = r_css && cl_abap_char_utilities=>newline.

    r_css = r_css && 'form {' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    background: #fff;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    margin: 6px;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    padding: 22px;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    border-radius: 10px;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    box-shadow: 0 2px 12px rgba(0,0,0,0.07);' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    width: 50%;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    max-width: 50%;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    min-width: 0;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    display: flex;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    flex-direction: column;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    align-items: stretch;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '}' && cl_abap_char_utilities=>newline.

    r_css = r_css && cl_abap_char_utilities=>newline.

    r_css = r_css && 'textarea {' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    min-height: 50px;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    padding: 5px;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    border: 1px solid #d1d5db;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    border-radius: 5px;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    font-size: 0.85rem;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    font-family: inherit;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    background: #f9fafb;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    resize: vertical;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    transition: border-color 0.2s, box-shadow 0.2s;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    box-shadow: 0 1px 2px rgba(0,0,0,0.03);' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    outline: none;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '}' && cl_abap_char_utilities=>newline.

    r_css = r_css && cl_abap_char_utilities=>newline.

    r_css = r_css && 'textarea:focus {' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    border-color: #2563eb;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    background: #fff;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    box-shadow: 0 0 0 2px #2563eb33;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '}' && cl_abap_char_utilities=>newline.

    r_css = r_css && cl_abap_char_utilities=>newline.

    r_css = r_css && 'input[type="password"]#apiKey {' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    border: 1px solid #d1d5db;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    border-radius: 5px;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    font-size: 0.85rem;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    font-family: inherit;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    background: #f9fafb;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    transition: border-color 0.2s, box-shadow 0.2s;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    box-shadow: 0 1px 2px rgba(0,0,0,0.03);' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    outline: none;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    margin-bottom: 10px;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '}' && cl_abap_char_utilities=>newline.

    r_css = r_css && cl_abap_char_utilities=>newline.

    r_css = r_css && 'input[type="password"]#apiKey:focus {' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    border-color: #2563eb;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    background: #fff;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    box-shadow: 0 0 0 2px #2563eb33;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '}' && cl_abap_char_utilities=>newline.

    r_css = r_css && cl_abap_char_utilities=>newline.

    r_css = r_css && 'button[type="submit"] {' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    margin-top: 8px;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    padding: 5px 16px;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    border: none;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    border-radius: 5px;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    background: #2563eb;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    color: #fff;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    font-size: 0.75rem;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    font-weight: 500;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    cursor: pointer;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    transition: background 0.18s;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    box-shadow: 0 1px 4px rgba(37,99,235,0.07);' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    align-self: flex-start;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    width: auto;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    min-width: 0;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '}' && cl_abap_char_utilities=>newline.

    r_css = r_css && cl_abap_char_utilities=>newline.

    r_css = r_css && 'button[type="submit"]:hover {' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    background: #1d4ed8;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '}' && cl_abap_char_utilities=>newline.

    r_css = r_css && cl_abap_char_utilities=>newline.

    r_css = r_css && '.new-chat-btn {' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    margin-left: 5px;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    margin-top: 8px;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    padding: 5px 16px;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    border: none;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    border-radius: 5px;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    background: #e5e7eb;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    color: #374151;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    font-size: 0.75rem;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    font-weight: 500;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    cursor: pointer;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    transition: background 0.18s;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    box-shadow: 0 1px 4px rgba(0,0,0,0.04);' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    align-self: flex-start;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    width: auto;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    min-width: 0;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '}' && cl_abap_char_utilities=>newline.

    r_css = r_css && cl_abap_char_utilities=>newline.

    r_css = r_css && '.new-chat-btn:hover {' && cl_abap_char_utilities=>newline.
    r_css = r_css && '    background: #d1d5db;' && cl_abap_char_utilities=>newline.
    r_css = r_css && '}' && cl_abap_char_utilities=>newline.

  ENDMETHOD.


  METHOD yif_aaic_ui_chat~get_html.

    TYPES: BEGIN OF ty_parts,
             text TYPE string,
           END OF ty_parts,

           ty_parts_t TYPE STANDARD TABLE OF ty_parts WITH DEFAULT KEY,

           BEGIN OF ty_msg,
             role    TYPE string,
             content TYPE string,
             text    TYPE string,
             parts   TYPE ty_parts_t,
           END OF ty_msg.

    DATA lt_anthropic_chat_response TYPE STANDARD TABLE OF yif_aaic_anthropic=>ty_response_content_s.

    DATA ls_msg TYPE ty_msg.

    DATA l_timestamp TYPE string.


    r_html = '<!DOCTYPE html>' && cl_abap_char_utilities=>newline.
    r_html = r_html && '<html lang="en">' && cl_abap_char_utilities=>newline.
    r_html = r_html && '<head>' && cl_abap_char_utilities=>newline.
    r_html = r_html && '<meta charset="UTF-8" />' && cl_abap_char_utilities=>newline.
    r_html = r_html && '<meta name="viewport" content="width=device-width, initial-scale=1.0" />' && cl_abap_char_utilities=>newline.
    r_html = r_html && '<title>ABAP AI Chat - OpenAI</title>' && cl_abap_char_utilities=>newline.
    r_html = r_html && '<style>' && cl_abap_char_utilities=>newline.

    "Get CSS
    r_html = r_html && me->get_css( ).

    r_html = r_html && '</style>' && cl_abap_char_utilities=>newline.
    r_html = r_html && '</head>' && cl_abap_char_utilities=>newline.
    r_html = r_html && '<body>' && cl_abap_char_utilities=>newline.
    r_html = r_html && '<div style="text-align: center">' && cl_abap_char_utilities=>newline.
    r_html = r_html && '<img src="https://christianjianelli.github.io/abapAI.svg" alt="Logo" style="height: 35px; margin-bottom: 10px" />' && cl_abap_char_utilities=>newline.
    r_html = r_html && '</div>' && cl_abap_char_utilities=>newline.
    r_html = r_html && '<div class="message-container">' && cl_abap_char_utilities=>newline.

    "Load chat messages history
    IF i_api IS NOT INITIAL AND i_chat_id IS NOT INITIAL.

      DATA(lo_aaic_db) = NEW ycl_aaic_db( i_api = i_api
                                          i_id = CONV #( i_chat_id ) ).

      lo_aaic_db->get_chat(
        EXPORTING
          i_ui = abap_true
        IMPORTING
          e_t_messages = DATA(lt_messages)
      ).

      LOOP AT lt_messages ASSIGNING FIELD-SYMBOL(<ls_message>).

        CLEAR ls_msg.

        /ui2/cl_json=>deserialize(
          EXPORTING
            json = <ls_message>-msg
          CHANGING
            data = ls_msg
        ).

        IF ls_msg-content IS INITIAL AND ls_msg-text IS NOT INITIAL.
          ls_msg-content = ls_msg-text.
        ENDIF.

        CASE i_api.

          WHEN yif_aaic_const=>c_anthropic.

            IF ls_msg-role = 'user'.

              FREE lt_anthropic_chat_response.

              /ui2/cl_json=>deserialize(
                EXPORTING
                  json = ls_msg-content
                CHANGING
                  data = lt_anthropic_chat_response
              ).

              IF lt_anthropic_chat_response IS NOT INITIAL.

                CLEAR ls_msg-content.

                LOOP AT lt_anthropic_chat_response ASSIGNING FIELD-SYMBOL(<ls_anthropic_chat_response>).

                  IF <ls_anthropic_chat_response>-type <> 'text'.
                    CONTINUE.
                  ENDIF.

                  ls_msg-content = <ls_anthropic_chat_response>-text.

                ENDLOOP.

              ELSE.

                /ui2/cl_json=>deserialize(
                 EXPORTING
                   json = ls_msg-content
                 CHANGING
                   data = ls_msg-content
               ).

              ENDIF.

            ELSE.

              FREE lt_anthropic_chat_response.

              /ui2/cl_json=>deserialize(
                EXPORTING
                  json = ls_msg-content
                CHANGING
                  data = lt_anthropic_chat_response
              ).

              LOOP AT lt_anthropic_chat_response ASSIGNING <ls_anthropic_chat_response>.

                IF <ls_anthropic_chat_response>-type <> 'text'.
                  CONTINUE.
                ENDIF.

                ls_msg-content = <ls_anthropic_chat_response>-text.

              ENDLOOP.

            ENDIF.

          WHEN yif_aaic_const=>c_google.

            LOOP AT ls_msg-parts ASSIGNING FIELD-SYMBOL(<ls_parts>).

              ls_msg-content = ls_msg-content && <ls_parts>-text.

            ENDLOOP.

        ENDCASE.

        IF ls_msg-content IS NOT INITIAL.

          CASE ls_msg-role.

            WHEN 'user'.

              r_html = r_html && '<div class="message user-message">' && cl_abap_char_utilities=>newline.
              r_html = r_html && '<div class="message-bubble">' && cl_abap_char_utilities=>newline.
              r_html = r_html && '<div class="markdown-content">' && cl_abap_char_utilities=>newline.
              r_html = r_html && '<p>' && ls_msg-content && '</p>' && cl_abap_char_utilities=>newline.
              r_html = r_html && '</div>' && cl_abap_char_utilities=>newline.
              r_html = r_html && '</div>' && cl_abap_char_utilities=>newline.
              r_html = r_html && |<div class="message-timestamp">{ <ls_message>-msg_date+6(2) }/{ <ls_message>-msg_date+4(2) }/{ <ls_message>-msg_date(4) } { <ls_message>-msg_time TIME = USER }</div> { cl_abap_char_utilities=>newline }|.
              r_html = r_html && '</div>' && cl_abap_char_utilities=>newline.

            WHEN 'assistant' OR 'model'.

              r_html = r_html && '<div class="message llm-message">' && cl_abap_char_utilities=>newline.
              r_html = r_html && '<div class="message-bubble">' && cl_abap_char_utilities=>newline.
              r_html = r_html && '<div class="markdown-content">' && cl_abap_char_utilities=>newline.
              r_html = r_html && ls_msg-content && cl_abap_char_utilities=>newline.
              r_html = r_html && '</div>' && cl_abap_char_utilities=>newline.
              r_html = r_html && '</div>' && cl_abap_char_utilities=>newline.
              r_html = r_html && |<div class="message-timestamp">{ <ls_message>-msg_date+6(2) }/{ <ls_message>-msg_date+4(2) }/{ <ls_message>-msg_date(4) } { <ls_message>-msg_time TIME = USER }</div> { cl_abap_char_utilities=>newline }|.
              r_html = r_html && '</div>' && cl_abap_char_utilities=>newline.

          ENDCASE.

        ENDIF.

      ENDLOOP.

    ENDIF.

    r_html = r_html && '</div>' && cl_abap_char_utilities=>newline.
    r_html = r_html && '<div class="form-container" style="display: flex; justify-content: center">' && cl_abap_char_utilities=>newline.
    r_html = r_html && '<form id="chatForm" method="post">' && cl_abap_char_utilities=>newline.

    r_html = r_html && '<input type="password" id="apiKey" name="apiKey" placeholder="Enter your API key" autocomplete="off"/>' && cl_abap_char_utilities=>newline.

    r_html = r_html && '<textarea id="mainText" name="mainText" maxlength="2000" placeholder="Enter your message here..."></textarea>' && cl_abap_char_utilities=>newline.
    r_html = r_html && '<div style="display: flex; flex-direction: row">' && cl_abap_char_utilities=>newline.
    r_html = r_html && '<button type="submit">Send</button>' && cl_abap_char_utilities=>newline.
    r_html = r_html && '<button type="button" class="new-chat-btn" id="newChatBtn">Clear</button>' && cl_abap_char_utilities=>newline.
    r_html = r_html && '</div>' && cl_abap_char_utilities=>newline.
    r_html = r_html && '</form>' && cl_abap_char_utilities=>newline.
    r_html = r_html && '</div>' && cl_abap_char_utilities=>newline.

    r_html = r_html && '<script src="https://cdn.jsdelivr.net/npm/marked/marked.min.js"></script>' && cl_abap_char_utilities=>newline.

    r_html = r_html && '<script>' && cl_abap_char_utilities=>newline.

    r_html = r_html && '  document.querySelectorAll(".message.user-message .message-bubble p").forEach((p) => { p.innerHTML = marked.parse(p.textContent); });' && cl_abap_char_utilities=>newline.

    r_html = r_html && '  document.querySelectorAll(".message.llm-message .message-bubble .markdown-content").forEach((el) => { el.innerHTML = marked.parse(el.innerHTML); });' && cl_abap_char_utilities=>newline.

    r_html = r_html && '  const userMessages = document.querySelectorAll(".user-message");' && cl_abap_char_utilities=>newline.

    r_html = r_html && '  if (userMessages.length > 0) { setTimeout(() => { userMessages[userMessages.length - 1].scrollIntoView({ behavior: "smooth", block: "start" }); }, 500); }' && cl_abap_char_utilities=>newline.

    r_html = r_html && '  const llmMessages = document.querySelectorAll(".llm-message");' && cl_abap_char_utilities=>newline.

    r_html = r_html && '  if (llmMessages.length > 0) { setTimeout(() => { llmMessages[llmMessages.length - 1].scrollIntoView({ behavior: "smooth", block: "start" }); }, 1000); }' && cl_abap_char_utilities=>newline.

    r_html = r_html && '  document.getElementById("chatForm").addEventListener("submit", function (e) {' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    e.preventDefault();' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    const mainText = document.getElementById("mainText").value.trim();' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    const apiKey = document.getElementById("apiKey").value.trim();' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    if (mainText && apiKey) {' && cl_abap_char_utilities=>newline.

    r_html = r_html && '      const timestamp = new Date().toLocaleString("en-GB", {' && cl_abap_char_utilities=>newline.
    r_html = r_html && '        year: "numeric",' && cl_abap_char_utilities=>newline.
    r_html = r_html && '        month: "2-digit",' && cl_abap_char_utilities=>newline.
    r_html = r_html && '        day: "2-digit",' && cl_abap_char_utilities=>newline.
    r_html = r_html && '        hour: "2-digit",' && cl_abap_char_utilities=>newline.
    r_html = r_html && '        minute: "2-digit",' && cl_abap_char_utilities=>newline.
    r_html = r_html && '        second: "2-digit",' && cl_abap_char_utilities=>newline.
    r_html = r_html && '        hour12: false' && cl_abap_char_utilities=>newline.
    r_html = r_html && '      });' && cl_abap_char_utilities=>newline.

    r_html = r_html && '      addUserMessage(mainText, timestamp);' && cl_abap_char_utilities=>newline.

    r_html = r_html && '      document.getElementById("mainText").value = "";' && cl_abap_char_utilities=>newline.

    r_html = r_html && '      showLlmTyping();' && cl_abap_char_utilities=>newline.
    r_html = r_html && '      sendUserPrompt(mainText, apiKey);' && cl_abap_char_utilities=>newline.

    r_html = r_html && '    }' && cl_abap_char_utilities=>newline.
    r_html = r_html && '  });' && cl_abap_char_utilities=>newline.

    r_html = r_html && '  document.getElementById("newChatBtn").addEventListener("click", function () {' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    const container = document.querySelector(".message-container");' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    container.innerHTML = "";' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    addLlmMessage("Welcome to the ABAP AI Chat! How can I assist you today?", new Date().toLocaleString());' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    document.getElementById("mainText").value = "";' && cl_abap_char_utilities=>newline.
    r_html = r_html && '  });' && cl_abap_char_utilities=>newline.

    r_html = r_html && '  const textarea = document.getElementById("mainText");' && cl_abap_char_utilities=>newline.

    r_html = r_html && '  textarea.addEventListener("input", function () { this.style.height = "auto"; this.style.height = this.scrollHeight + "px"; });' && cl_abap_char_utilities=>newline.

    r_html = r_html && '  function addLlmMessage(message, timestamp) {' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    const container = document.querySelector(".message-container");' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    const div = document.createElement("div");' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    div.className = "message llm-message";' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    div.innerHTML = `<div class="message-bubble"><div class="markdown-content"></div></div><div class="message-timestamp">${timestamp}</div>`;' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    container.appendChild(div);' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    const markdownDiv = div.querySelector(".markdown-content");' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    markdownDiv.innerHTML = marked.parse(message);' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    setTimeout(() => { div.scrollIntoView({ behavior: "smooth", block: "start" }); }, 300);' && cl_abap_char_utilities=>newline.
    r_html = r_html && '  }' && cl_abap_char_utilities=>newline.

    r_html = r_html && '  function addUserMessage(message, timestamp) {' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    const container = document.querySelector(".message-container");' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    const div = document.createElement("div");' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    div.className = "message user-message";' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    div.innerHTML = `<div class="message-bubble"><p>${message}</p></div><div class="message-timestamp">${timestamp}</div>`;' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    container.appendChild(div);' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    const p = div.querySelector(".message-bubble p");' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    p.innerHTML = marked.parse(p.innerHTML);' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    setTimeout(() => { div.scrollIntoView({ behavior: "smooth", block: "start" }); }, 300);' && cl_abap_char_utilities=>newline.
    r_html = r_html && '  }' && cl_abap_char_utilities=>newline.

    r_html = r_html && '  window.addEventListener("DOMContentLoaded", function () {' && cl_abap_char_utilities=>newline.

    IF lt_messages IS INITIAL.
      r_html = r_html && '    addLlmMessage("Welcome to the ABAP AI Chat! How can I assist you today? 😊", "");' && cl_abap_char_utilities=>newline.
    ENDIF.

    r_html = r_html && '  });' && cl_abap_char_utilities=>newline.

    r_html = r_html && '  function sendUserPrompt(userPrompt, apikey) {' && cl_abap_char_utilities=>newline.
    r_html = r_html && '        const url = window.location.href; // Replace with your actual endpoint' && cl_abap_char_utilities=>newline.
    r_html = r_html && '        const data = {' && cl_abap_char_utilities=>newline.
    r_html = r_html && '            chatId: "' && i_chat_id && '",' && cl_abap_char_utilities=>newline.
    r_html = r_html && '            apikey: apikey,' && cl_abap_char_utilities=>newline.
    r_html = r_html && '            prompt: userPrompt' && cl_abap_char_utilities=>newline.
    r_html = r_html && '        };' && cl_abap_char_utilities=>newline.

    r_html = r_html && '        fetch(url, {' && cl_abap_char_utilities=>newline.
    r_html = r_html && '            method: "POST",' && cl_abap_char_utilities=>newline.
    r_html = r_html && '            headers: {' && cl_abap_char_utilities=>newline.
    r_html = r_html && '            "Content-Type": "application/json"' && cl_abap_char_utilities=>newline.
    r_html = r_html && '            },' && cl_abap_char_utilities=>newline.
    r_html = r_html && '            body: JSON.stringify(data)' && cl_abap_char_utilities=>newline.
    r_html = r_html && '        })' && cl_abap_char_utilities=>newline.
    r_html = r_html && '        .then(response => {' && cl_abap_char_utilities=>newline.
    r_html = r_html && '            if (!response.ok) {' && cl_abap_char_utilities=>newline.
    r_html = r_html && '            throw new Error("HTTP error! Status: ${response.status}");' && cl_abap_char_utilities=>newline.
    r_html = r_html && '            }' && cl_abap_char_utilities=>newline.
    r_html = r_html && '            return response.json();' && cl_abap_char_utilities=>newline.
    r_html = r_html && '        })' && cl_abap_char_utilities=>newline.
    r_html = r_html && '        .then(result => {' && cl_abap_char_utilities=>newline.
    r_html = r_html && '            console.log("Server response:", result);' && cl_abap_char_utilities=>newline.

    r_html = r_html && '            const timestamp = new Date().toLocaleString("en-GB", {' && cl_abap_char_utilities=>newline.
    r_html = r_html && '              year: "numeric",' && cl_abap_char_utilities=>newline.
    r_html = r_html && '              month: "2-digit",' && cl_abap_char_utilities=>newline.
    r_html = r_html && '              day: "2-digit",' && cl_abap_char_utilities=>newline.
    r_html = r_html && '              hour: "2-digit",' && cl_abap_char_utilities=>newline.
    r_html = r_html && '              minute: "2-digit",' && cl_abap_char_utilities=>newline.
    r_html = r_html && '              second: "2-digit",' && cl_abap_char_utilities=>newline.
    r_html = r_html && '              hour12: false' && cl_abap_char_utilities=>newline.
    r_html = r_html && '            });' && cl_abap_char_utilities=>newline.

    r_html = r_html && '            removeLlmTyping();' && cl_abap_char_utilities=>newline.
    r_html = r_html && '            addLlmMessage(result.message, timestamp);' && cl_abap_char_utilities=>newline.

    r_html = r_html && '        })' && cl_abap_char_utilities=>newline.
    r_html = r_html && '        .catch(error => {' && cl_abap_char_utilities=>newline.
    r_html = r_html && '            console.error("Request failed:", error);' && cl_abap_char_utilities=>newline.
    r_html = r_html && '        });' && cl_abap_char_utilities=>newline.
    r_html = r_html && '  }' && cl_abap_char_utilities=>newline.

    r_html = r_html && '  /**' && cl_abap_char_utilities=>newline.
    r_html = r_html && '   * Appends the LLM typing indicator to the message-container.' && cl_abap_char_utilities=>newline.
    r_html = r_html && '   */' && cl_abap_char_utilities=>newline.
    r_html = r_html && '  function showLlmTyping() {' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    // Prevent multiple indicators' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    if (document.getElementById("llm-typing")) return;' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    const container = document.querySelector(".message-container");' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    const typingDiv = document.createElement("div");' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    typingDiv.id = "llm-typing";' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    typingDiv.className = "message llm-message";' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    typingDiv.innerHTML = `' && cl_abap_char_utilities=>newline.
    r_html = r_html && '      <div class="message-bubble">' && cl_abap_char_utilities=>newline.
    r_html = r_html && '        <div class="llm-typing">' && cl_abap_char_utilities=>newline.
    r_html = r_html && '          <div class="llm-typing-dot"></div>' && cl_abap_char_utilities=>newline.
    r_html = r_html && '          <div class="llm-typing-dot"></div>' && cl_abap_char_utilities=>newline.
    r_html = r_html && '          <div class="llm-typing-dot"></div>' && cl_abap_char_utilities=>newline.
    r_html = r_html && '        </div>' && cl_abap_char_utilities=>newline.
    r_html = r_html && '      </div>' && cl_abap_char_utilities=>newline.
    r_html = r_html && '      <div class="message-timestamp"></div>' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    `;' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    container.appendChild(typingDiv);' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    setTimeout(() => {' && cl_abap_char_utilities=>newline.
    r_html = r_html && '      typingDiv.scrollIntoView({ behavior: "smooth", block: "start" });' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    }, 100);' && cl_abap_char_utilities=>newline.
    r_html = r_html && '  }' && cl_abap_char_utilities=>newline.

    r_html = r_html && '  /**' && cl_abap_char_utilities=>newline.
    r_html = r_html && '   * Removes the LLM typing indicator from the message-container.' && cl_abap_char_utilities=>newline.
    r_html = r_html && '   */' && cl_abap_char_utilities=>newline.
    r_html = r_html && '  function removeLlmTyping() {' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    const typingDiv = document.getElementById("llm-typing");' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    if (typingDiv) {' && cl_abap_char_utilities=>newline.
    r_html = r_html && '      typingDiv.remove();' && cl_abap_char_utilities=>newline.
    r_html = r_html && '    }' && cl_abap_char_utilities=>newline.
    r_html = r_html && '  }' && cl_abap_char_utilities=>newline.

    r_html = r_html && '</script>' && cl_abap_char_utilities=>newline.
    r_html = r_html && '</body>' && cl_abap_char_utilities=>newline.
    r_html = r_html && '</html>' && cl_abap_char_utilities=>newline.

  ENDMETHOD.
ENDCLASS.
