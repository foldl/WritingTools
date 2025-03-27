# Writing Tools

https://github.com/user-attachments/assets/6a530cd7-2953-4a89-ac87-ca3b40e7b07e


## 🚀 What is it?

Writing Tools, Apple's AI-inspired app, enchants Windows, enhancing your pen with AI LLMs.
One hotkey press, system-wide, fixes grammar, punctuation, and more. It's the world's most intelligent, system-wide grammar assistant.

Based on [ChatLLM.cpp](https://github.com/foldl/chatllm.cpp), and built with [Delphi](https://www.embarcadero.com/products/delphi) and [Lazarus](https://www.lazarus-ide.org/).

🚀🚀🚀 Don't forget to update ChatLLM.cpp bindings to enjoy GPU acceleration!

### 🌟 Why Choose Writing Tools?

>_In the realm of AI, a secret lies,</br> Where LLMs run on your very site.</br> System-wide, without a hitch,</br> Instantly in any app you might pick._</br>
>
>_Clipboard untouched, a pure delight,</br> No need to worry, no data to steal.</br> Purely native, no Python, no Java, no JS,</br> Bloat-free, CPU usage, it's a joke._</br>
>
>_Chat mode, a quick query's delight,</br> No text selected, a chat mode you'll find.</br> For quick queries, assistance at your command,</br> All commands customizable, a true delight._</br>
>
>_Privacy, absolute, a promise kept,</br>No data to share, no worries to find.</br> Free and open-source, a pure delight,</br> Purely native, no bloat, no pain._</br>
>
>_In the world of AI, a secret lies,</br>Where LLMs run on your very site._</br>


- **_Absolute_ privacy**: Based on ChatLLM.cpp. LLMs run on your machine.
- **System-wide Functionality**: Works instantly in **any application** where you can select text. **Does not overwrite your clipboard**.
- **Completely free and Open-source**: Purely native. Bloat-free & uses pretty much **0% of your CPU**.
- **Chat Mode**: Seamlessly switching between context processing mode and chat mode.
- **Customization**: All commands are fully customizable.
- **Markdown Rendering**: Beautiful and elegant.

## ✨ Features

All features are defined by users based on their own needs. Some examples.

- **Proofread**: The smartest grammar and spelling corrector.
- **Rewrite**: Improve the phrasing of your text.
- **Make Friendly/Professional**: Adjust the tone of your writing.
- **Summarize**: Create concise summaries of longer texts.
- **Create Tables**: Convert text into a structured Markdown table.
- **Custom Instructions**: Give specific directions (e.g. `Translate to Chinese`).

Invoke Writing Tools with no text selected to enter quick chat mode.

## 🛠 Quick Start

1. Go to the [Releases](https://github.com/foldl/WritingTools/releases) page and download the latest package.

1. Extract it anywhere you want.

1. Download a quantized model for [ChatLLM.cpp](https://github.com/foldl/chatllm.cpp). Some small models:

   |Model name        | Size (GB)      |   Link  |
   |:----------------:|---------------:|:--------------|
   | QWen-2.5 1.5B    |      1.6       |[Link](https://modelscope.cn/api/v1/models/judd2024/chatllm_quantized_qwen2.5/repo?Revision=master&FilePath=qwen2.5-1.5b.bin)  |
   | Gemma-2 2B       |      2.8       |[Link](https://modelscope.cn/api/v1/models/judd2024/chatllm_quantized_gemma2_2b/repo?Revision=master&FilePath=gemma2-2b.bin)  |

1. Configure your profile.

   Copy `profile.json.in` to `profile.json`. Fill in the path of the quantized model file:

   ```js
   {
      //...
      "chatllm": {
         "default": [
            "-m",
            "path of the quantized model file"
         ]
      },
      //...
   }
   ```

   `profile.json` shall be encoded in **UTF-8**.

1. Start `WritingTools.exe`;

### Context-aware Assistance

1. Select any text in any application (or don't select any text to use quick chat mode).

2. Press your hotkey (Default: _Win+Shift+I_).

3. Choose an option from the popup menu or enter a custom instruction.

### Quick Chat

Select nothing in other applications and press your hotkey will enter quick chat mode. Enter a prompt and start chatting.

Even if some text is selected, you can input a prompt starting with "/chat " or "chat :" to switch to chat mode.

## ⚙　Customization

Fully customizable through `profile.json`.

### LLM

Multiple LLMs can be defined and loaded simultaneously. Each is defined as an entry in `chatllm`:

```js
{
   //...
   "chatllm": {
      "default": [...],
      "another_one": [...]
   },
   //...
}
```

`default` is the default LLM for actions, which can be omitted when defining actions.

### Actions

_Actions_ are represented to users as a collection of buttons. Each action is defined as a dictionary:

```js
{
   "name": "My Action",
   "prompt": "Check this:\n\n{context}",
   "sys_prompt": "....",
   "accelerator": "p",        // optional
   "llm": "another_one",      // optional
   "ai_prefix": "...",        // optional
   "ai_suffix": "...",        // optional
   "action": "show"           // optional
}
```

* `name` gives the caption of the button.
* `prompt` is the prompt fed to the LLM, in which `{context}` represents the selected text.
* `sys_prompt` is the system prompt fed to the LLM.
* `accelerator` is the accelerator of the button (single char, optional).
* `llm` is selected LLM to serve this action (when omitted, "default" is selected).
* `ai_prefix` is used for [generation steering](https://github.com/foldl/chatllm.cpp/blob/master/docs/fun.md#generation-steering).
* `ai_suffix` is used abort generation: once this suffix is found in LLM's output, generation is aborted.
* `action` is the post action to handle the output of LLM. Possible values:

   - `show`: show the output in a box.
   - `prepend`: prepend the output in front of current selection.
   - `replace`: replace the current selection by the output (**default**).
   - `append`: append the output following current selection.
   - `clipboard`: copy the output to the clipboard.

An example of `ai_prefix` and `ai_suffix`: **force** AI to generate **just** doxgen-style comments for functions.

```js
{
   "ai_prefix": "\/** @brief",
   "ai_suffix": "*\/",
   // ...
}
```

A list of such actions are defined under `actions` in `profile.json`.

### Special Actions

There are two special actions, one is defined under `custom` in `profile.json`, which defines the behavior when
users input some custom instruction; the other one is defined under `quick-chat`, which defines the behavior for quick chat mode.

`action` of a custom instruction can be configured on-the-fly by adding a prefix to the instruction, e.g. to select the `show` action:

* "/show explain this", or
* "show: explain this"


### UI

Users can change hotkey, title and background. Note: _width_ and _height_ are be saved automatically when resized.

```js
{
   "title": "My Writing Tools",
   "hotkey": "win+shift+I",
   "ui": {
      "width": 587,
      "background": {
         "color1": "ccaaaa",
         "color2": "ffffff"
      },
      "height": 422
   },
   //...
}
```

Some vibrant gradients:

|These|vibrant|gradients|exhibit|a|very|sweet|appearance|
|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
|2c91fe, 5f21df</br><img src="media/5f21df_2c91fe.png" width="64px"></img>|2aa4fe, 2497e7</br><img src="media/2497e7_2aa4fe.png" width="64px"></img>|f0cb79, c79f35</br><img src="media/c79f35_f0cb79.png" width="64px"></img>|cc9621, 6d16be</br><img src="media/6d16be_cc9621.png" width="64px"></img>|cfcb3d, 3dc7de</br><img src="media/3dc7de_cfcb3d.png" width="64px"></img>|b979f0, 7a2be0</br><img src="media/7a2be0_b979f0.png" width="64px"></img>|edcb77, c8730d</br><img src="media/c8730d_edcb77.png" width="64px"></img>|cbc445, 6681e3</br><img src="media/6681e3_cbc445.png" width="64px"></img>|
|35d2fa, b6ea9c</br><img src="media/b6ea9c_35d2fa.png" width="64px"></img>|483af3, 7010a4</br><img src="media/7010a4_483af3.png" width="64px"></img>|cc9211, ba16e0</br><img src="media/ba16e0_cc9211.png" width="64px"></img>|a1450a, c3a822</br><img src="media/c3a822_a1450a.png" width="64px"></img>|08b3d3, 23eef9</br><img src="media/23eef9_08b3d3.png" width="64px"></img>|94c37e, c3b72f</br><img src="media/c3b72f_94c37e.png" width="64px"></img>|5b2af0, eaac65</br><img src="media/eaac65_5b2af0.png" width="64px"></img>|651ed8, 375af7</br><img src="media/375af7_651ed8.png" width="64px"></img>|
|4919e3, ead0ab</br><img src="media/ead0ab_4919e3.png" width="64px"></img>|f3b971, ba23ee</br><img src="media/ba23ee_f3b971.png" width="64px"></img>|35b1f7, 203ffa</br><img src="media/203ffa_35b1f7.png" width="64px"></img>|daf871, cea8f0</br><img src="media/cea8f0_daf871.png" width="64px"></img>|5dc3ee, da999b</br><img src="media/da999b_5dc3ee.png" width="64px"></img>|820ead, b453ea</br><img src="media/b453ea_820ead.png" width="64px"></img>|0816ac, 403eee</br><img src="media/403eee_0816ac.png" width="64px"></img>|82b5fe, 7526ff</br><img src="media/7526ff_82b5fe.png" width="64px"></img>|
|833d1f, c29300</br><img src="media/c29300_833d1f.png" width="64px"></img>|ab9c38, 4abcc2</br><img src="media/4abcc2_ab9c38.png" width="64px"></img>|eea82d, 3adbbb</br><img src="media/3adbbb_eea82d.png" width="64px"></img>|8e4b5a, 6d12f1</br><img src="media/6d12f1_8e4b5a.png" width="64px"></img>|3ec9e4, 71d3a5</br><img src="media/71d3a5_3ec9e4.png" width="64px"></img>|bb765b, 2c60f0</br><img src="media/2c60f0_bb765b.png" width="64px"></img>|d73d96, bb661e</br><img src="media/bb661e_d73d96.png" width="64px"></img>|462a95, c25c21</br><img src="media/c25c21_462a95.png" width="64px"></img>|

### Troubleshooting (Fine Tuning)

1. Clipboard not working?

   These three delays in milliseconds specify how to simulate Ctrl+C and detect clipboard changes.

   ```js
   {
      "ui": {
         ...
         "hotkey": {
            "delay1": 200, // before simulating Ctrl+C
            "delay2": 40,  // between key inputs within Ctrl+C
            "delay3": 100  // wait for clipboard changed after Ctrl+C
         },
      },
      //...
   }
   ```

1. App can't load?

   WebView2 runtime is required, which is preinstalled onto all Windows 11 and most of Windows 10. If problems are encountered,
   check the [document](https://developer.microsoft.com/en-us/microsoft-edge/webview2/?form=MA13LH) and install it.
   [Link](https://go.microsoft.com/fwlink/p/?LinkId=2124703)

#### Know Issues

1. Reading of current selection through clipboard is unreliable. Please re-try.

### Auto Run

Add a shortcut of the `WritingTools.exe` to the Windows Start-Up folder.

## 👨‍💻 To compile the application yourself:

Precondition: Build `libchatllm` or get `libchatllm.dll` & `ggml.dll` from releases;

### Delphi

1. Install [Delphi Community Edition](https://www.embarcadero.com/products/delphi/starter/free-download/);
1. Build this project (Target: Win64);
1. Copy `libchatllm.dll` & `ggml.dll` to the output directory (such as _Win64/Debug_).

### Lazarus

1. Install [Lazarus](https://www.lazarus-ide.org/) Win64;
1. Install package [WebView4Delphi](https://github.com/salvadordf/WebView4Delphi) to Lazarus;
1. Build this project (_LazWritingTools.lpi_);
1. Copy `libchatllm.dll` & `ggml.dll` to the output directory (such as _lib/x86\_64-win64_).

## 👏‍ Acknowledgements

* This project is inspired by another [WritingTools](https://github.com/theJayTea/WritingTools).
   Let's keep things simple, with Delphi.

* [Super Object Toolkit](https://github.com/hgourvest/superobject) for JSON manipulation.

* [Markdown Processor](https://github.com/grahamegrieve/delphi-markdown) for Markdown rendering.

* [Prism](https://prismjs.com) for code highlighting.

## 📄 License

Distributed under the MIT License.