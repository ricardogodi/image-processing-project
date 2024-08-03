## Project Overview

### Description

This project processes PPM image files, performing operations like grayscale conversion, thresholding, flipping horizontally, edge detection, and rotating right by 90 degrees. The project is written in F# and uses functional programming techniques to manipulate the images. C# is used as a bridge to call the F# functions.

### Contributions

The C# framework was provided by Prof. Joe Hummel and later modified by Pat Troy, UIC. My contribution as a student was to create the image processing functions found in `Library.fs`.

### Instructions

#### Prerequisites

- .NET 8 SDK installed

#### Setup

1. Clone the repository:

    ```sh
    git clone https://github.com/ricardogodi/image-processing-project.git
    cd yourrepository
    ```

2. Ensure .NET 8 SDK is installed:

    ```sh
    dotnet --version
    ```

    If not installed, download and install from the official [.NET download page](https://dotnet.microsoft.com/download/dotnet/8.0).

3. Build the project:

    ```sh
    make build
    ```

4. Run the project:

    ```sh
    make run
    ```

### Usage

1. When prompted, enter the filename of the PPM image file. A sample file, `cake.ppm`, is provided for demonstration.
2. Choose an operation:
    - `1` => grayscale
    - `2` => threshold
    - `3` => flip horizontal
    - `4` => edge detect
    - `5` => right-rotate 90
