# Learning Ollama

Purpose: A folder to keep notes and to maintain code for learning  Ollama

Author: Chaitanya Anand<br>
Created: Jan 31, 2025<br>
Last modified: Jan 31, 2025<br>

----

## What is Ollama?
Ollama is a program that can run on a local computer and can be used to manage and work with large language models. Ollama serves the following main functions
1. Allows one to manage and switch between various open source LLMs
2. Runs the models locally on the host computer and allows for interacting with them either through the command line interface or programatically via APIs
3. It provides a unified interface for working with different models from various sources, hence serving as an abstraction layer between the models themselves and the applications that use them

## How to use Ollama? (CLI)
Ollama runs as a server on the local computer. Before we can start working with Ollama, we need to start the program using the commmand<br>
`ollama serve`

Now we have the Ollama program running on our computer, we now need to load a model. We shall start with the llama3.2 model (to see a list of all available models visit https://ollama.com/search). To do so, in a new terminal window type:<br>
`ollama run llama3.2`

If running a particular model for the first time, ollama will automatically download the model before loading it.<br>

llama3.2 is a relatively light weight model that comes in the 3 billion and 1 billion parameter versions. running llama3.2 will by defaut run the 3b parameter model. To specify the version we can use `ollama run llama3.2:3b` / `ollama run llama3.2:1b`. The LLM model should now be running and interactable via the commandline.

When running model, we can use the folowing commands to control the session<br>
`/set` - set environment variables for the chat session<br>
`/show` - Show model information<br>
`/load <model>` - Load a session or model<br>
`/save <model>` - Save your current session<br>
`/clear` - Clear session context<br>
`/bye` - Exit the session<br>
`/?, /help` - Help for a command<br>
`/? shortcuts` - Help for keyboard shortcuts<br>

To stop running a model just type `/bye` in the model prompt<br>
To stop the ollama server, go the terminal where we started the ollama server and press `ctrl + c`<br>

## Managing models
We can manage all the models available to us using<br>
`ollama rm <model_name>` - remove a model<br>
`ollama list` - list all available models<br>
