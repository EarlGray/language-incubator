# macos hardened runtime jit howto

Create a local developer certificate (this is not a distribution certificate!):

- install Xcode, run it, go to "Setting" -> "Accounts", sign in with your Apple ID, select it, click "Manage Certificates", add a new one.for Apple Development. Once your developer certificates shows in the `login` key chain in _Keychain Access_, mark it "Always Trust" for "Code Signing" in 'Get Info'. Note down any recognizable part of your certifite name (e.g. your Apple ID).

- Download the [Apple Worldwide Developer Relations - G3](https://www.apple.com/certificateauthority/AppleWWDRCAG3.cer) intermediate certificate, run _Keychain Access_, import this certificate into your `login` keychain. In 'Get Info' -> 'Trust', mark it "Always Trust" for "Code Signing".

- `cargo build`/`cargo build --release` to build the executables

- `mac/sign.sh $YOUR_DEVID` to sign them (this will ask for password for every executable).

Enjoy.
