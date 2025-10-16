# yaai_cloud - ABAP AI tools Cloud
This repository provides a set of tools designed to facilitate Artificial Intelligence capabilities within **ABAP Cloud** environments.

## Key Features
- **Multi-Provider LLM API Support**: ABAP AI tools Cloud supports seamless integration with multiple large language model APIs, giving you flexibility in choosing your provider and deployment model. The list of supported APIs will continue to grow as the project evolves. Currently supported APIs include:
  - **[OpenAI](openai/README.md)**
  - **[Google Gemini](google/README.md)**
  - **[Anthropic](anthropic/README.md)** 
  - APIs from other providers, such as Mistral, that implement the OpenAI API standards also work with ABAP AI tools Cloud, possibly requiring minor adjustments.
    - **[Mistral](openai_compatible/README.md)** (cloud-based)
- **Conversation Management**: Maintain and manage multi-turn conversations, with full access to conversation history for advanced chat scenarios.
- **Tool/Function Calling**: Integrate ABAP business logic with LLMs using function/tool calling, allowing AI models to trigger ABAP methods.
- **Retrieval-Augmented Generation (RAG)**: Enhance LLM outputs by incorporating enterprise data, enabling more accurate and relevant answers through retrieval-augmented generation workflows.

These features empower you to build intelligent, enterprise-ready ABAP applications that leverage the latest advancements in AI.

  **Disclaimer:** ABAP AI tools Cloud is experimental and released under the MIT License. It is provided "as is", without warranty of any kind, express or implied. This means you use these tools at your own risk, and the authors are not liable for any damages or issues arising from their use.

## Prerequisites
 - **SAP ABAP Cloud**: You need an SAP BTP ABAP environment or SAP S/4HANA Cloud ABAP environment (a.k.a. Embedded Steampunk).
 - **abapGit**: Ensure that `abapGit Repositories Eclipse ADT Plug-In` is installed. You can find all information on the SAP Help website: https://help.sap.com/docs/btp/sap-business-technology-platform/working-with-abapgit?locale=en-US

## User Interface - ABAP AI Chat
In a BTP ABAP environment, the user interface must be a web application. ABAP AI tools Cloud includes a basic chat interface that you can use immediately or create a copy and modify it to fit your requirements.

While ABAP AI tools Cloud does not directly provide a SAPUI5 or SAP Fiori Elements chat user interface, this documentation explains how to integrate an ABAP AI chat into a SAPUI5 Freestyle application or a SAP Fiori Elements application.

### HTTP Services
ABAP AI tools Cloud provides several HTTP services, each offering a basic chat user interface for interacting with different LLM providers and models.

**Available Chat UIs:**
1. **OpenAI Chat UI** – Interact with OpenAI models through a simple web-based chat interface.
2. **Anthropic Chat UI** – Communicate with Anthropic models using a dedicated chat UI.
3. **Google Gemini Chat UI** – Access Google Gemini models via an integrated chat interface.
4. **Mistral Chat UI** – Connect to Mistral models with a compatible chat UI.

You can use these UIs as-is, or create a copy and modify it to fit your specific requirements.

### SAPUI5 Freestyle Application
Since SAPUI5 does not have dedicated controls to build a chat interface, we need to build a custom one. This documentation provides instructions and code for adding a basic chat interface to a SAPUI5 Freestyle application. It is a simple starting point so you can quickly connect your app to the ABAP AI tools.   

### SAP Fiori Elements Application
Adding a chat to a SAP Fiori Elements Application is a more complex task. It requires a controller extension. 


## Get Started

  - [Installation](installation.md): See how to install ABAP AI.
  - [Quickstart](quickstart.md): Run your first ABAP AI applications.
  - [Configuration](configuration.md): See how to configure Connections.

## APIs

  - [OpenAI Guide](openai/README.md): Learn how to use ABAP AI with OpenAI models.
  - [Google Gemini Guide](google/README.md): Learn how to use ABAP AI with Google Gemini models.
  - [Anthropic Guide](anthropic/README.md): Learn how to use ABAP AI with Anthropic models.
  - [OpenAI Compatible](openai_compatible/README.md): Learn how to use ABAP AI with OpenAI compatible APIs (like Mistral).

## Features
- [System Instructions](system_instructions.md): Learn how to pass System Instructions to a LLM.
- [Prompt Templates](prompt_templates.md): Learn how to use Prompt Templates.
- [Function Calling](function_calling.md): Learn how to use Function Calling.
- [Retrieval-Augmented Generation (RAG)](rag.md): Enhance your LLM applications by incorporating external knowledge sources and retrieval mechanisms.