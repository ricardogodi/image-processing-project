//
// F# image processing functions.
//
// This program performs the following tasks using images:
// - Grayscale
// - Threshold
// - FlipHorizontal
// - EdgeDetect
// - Rotate90
//
// Author: Ricardo Gonzalez
// University of Illinois at Chicago
//
// CS 341, Fall 2022
// 
// Professor: Patrick Troy

namespace ImageLibrary
module Operations =

  //
  // Grayscale helper function
  //
  let rec nextRow_Grayscale (row: (int*int*int) list) =
    match row with
    | []            -> []
    | (R, G, B)::tl -> let sum = int ((float R)*0.299 + (float G)*0.587 + (float B)*0.114) 
                       (sum,sum,sum)::(nextRow_Grayscale tl)
  //
  // Grayscale:
  //
  // Converts the image into grayscale and returns the 
  // resulting image as a list of lists. Pixels in grayscale
  // have the same value for each of the Red, Green and Blue
  // values in the RGB value.  Conversion to grayscale is done
  // by using a WEIGHTED AVERAGE calculation.  A normal average
  // (adding the three values and dividing by 3) is not the best,
  // since the human eye does not perceive the brightness of 
  // red, green and blue the same.  The human eye perceives 
  // green as brighter than red and it perceived red as brighter
  // than blue.  Research has shown that the following weighted
  // values should be used when calculating grayscale.
  //  - the green value should account for 58.7% of the grayscale.
  //  - the red value should account for   29.9% of the grayscale.
  //  - the blue value should account for  11.4% of the grayscale.
  //
  // So if the RGB values were (25, 75, 250), the grayscale amount 
  // would be 80, (25 * 0.299 + 75 * 0.587 + 250 * 0.114 => 80)
  // and then all three RGB values would become 80 or (80, 80, 80).
  // We will use truncation to cast from the floating point result 
  // to the integer grayscale value.
  //
  // Returns: updated image.
  //
  let rec Grayscale (width:int) 
                    (height:int) 
                    (depth:int) 
                    (image:(int*int*int) list list) = 

    match image with
    | []     -> []
    | hd::tl -> let nextRow = nextRow_Grayscale hd
                nextRow::(Grayscale width height depth tl)

  //
  // Threshold helper function 2
  //
  let nextColor_Threshold (value:int) 
                          (threshold:int) 
                          (depth:int) =
    if value > threshold then
      depth
      
    else  
      0

  //
  // Threshold helper function 1
  //
  let rec nextRow_Threshold (row:(int*int*int) list) 
                            (threshold:int) 
                            (depth:int) =
    match row with
    | []            -> []
    | (R, G, B)::tl -> let newR = nextColor_Threshold R threshold depth
                       let newG = nextColor_Threshold G threshold depth
                       let newB = nextColor_Threshold B threshold depth
                       (newR,newG,newB)::(nextRow_Threshold tl threshold depth)

  //
  // Threshold
  //
  // Thresholding increases image separation --- dark values 
  // become darker and light values become lighter. Given a 
  // threshold value in the range 0 < threshold < color depth,
  // each RGB value is compared to see if it's > threshold.
  // If so, that RGB value is replaced by the color depth;
  // if not, that RGB value is replaced with 0. 
  //
  // Example: if threshold is 100 and depth is 255, then given 
  // a pixel (80, 120, 160), the new pixel is (0, 255, 255).
  //
  // Returns: updated image.
  //
  let rec Threshold (width:int) 
                    (height:int)
                    (depth:int)
                    (image:(int*int*int) list list)
                    (threshold:int) = 
    match image with
    | []     -> []
    | hd::tl -> let nextRow = nextRow_Threshold hd threshold depth
                nextRow::(Threshold width height depth tl threshold)



  //
  // FlipHorizontal helper function
  //
  let rec nextRow_FlipHorizontal (row: (int*int*int) list) =
   List.rev row

  //
  // FlipHorizontal:
  //
  // Flips an image so that what’s on the left is now on 
  // the right, and what’s on the right is now on the left. 
  // That is, the pixel that is on the far left end of the
  // row ends up on the far right of the row, and the pixel
  // on the far right ends up on the far left. This is 
  // repeated as you move inwards toward the row's center.
  //
  // Returns: updated image.
  //
  let rec FlipHorizontal (width:int)
                         (height:int)
                         (depth:int)
                         (image:(int*int*int) list list) = 
    match image with
    | []     -> []
    | hd::tl -> let nextRow = nextRow_FlipHorizontal hd
                nextRow::(FlipHorizontal width height depth tl)



  //
  // traverse helper function  
  //

  let rec traverse list1 list2 threshold =
    match list1, list2 with
    | hd::[],_          -> []
    | hd1::tl1,hd2::tl2 -> let (R1A,G1A,B1A) = hd1
                           let (R1B,G1B,B1B) = List.head tl1
                           let (R2A,G2A,B2A) = hd2
                           //let (R2B,G2B,B2B) = List.head tl2
                           let result1 = sqrt((float (R1A - R1B))**2.0 + (float (G1A - G1B))**2.0 + (float (B1A - B1B))**2.0)
                           let result2 = sqrt((float (R1A - R2A))**2.0 + (float (G1A - G2A))**2.0 + (float (B1A - B2A))**2.0)

                           if ((result1 > float threshold) || (result2 > float threshold)) then
                             
                              (0,0,0)::(traverse tl1 tl2 threshold)
                           else 
                              (255,255,255)::(traverse tl1 tl2 threshold)
                          

  //
  // Edge Detection:
  //
  // Edge detection is an algorithm used in computer vision to help
  // distinguish different objects in a picture or to distinguish an
  // object in the foreground of the picture from the background.
  //
  // Edge Detection replaces each pixel in the original image with
  // a black pixel, (0, 0, 0), if the original pixel contains an 
  // "edge" in the original image.  If the original pixel does not
  // contain an edge, the pixel is replaced with a white pixel 
  // (255, 255, 255).
  //
  // An edge occurs when the color of pixel is "signigicantly different"
  // when compared to the color of two of its neighboring pixels. 
  // We only compares each pixel in the image with the 
  // pixel immediately to the right of it and with the pixel
  // immediately below it. If either pixel has a color difference
  // greater than a given threshold, then it is "significantly
  // different" and an edge occurs. Note that the right-most column
  // of pixels and the bottom-most column of pixels can not perform
  // this calculation so the final image contain one less column
  // and one less row than the original image.
  //
  // To calculate the "color difference" between two pixels, we
  // treat the each pixel as a point on a 3-dimensional grid and
  // we calculate the distance between the two points using the
  // 3-dimensional extension to the Pythagorean Theorem.
  // Distance between (x1, y1, z1) and (x2, y2, z2) is
  //  sqrt ( (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2 )
  //
  // The threshold amount will need to be given, which is an 
  // integer 0 < threshold < 255.  If the color distance between
  // the original pixel either of the two neighboring pixels 
  // is greater than the threshold amount, an edge occurs and 
  // a black pixel is put in the resulting image at the location
  // of the original pixel. 
  //
  // Returns: updated image.
  //
  let rec EdgeDetect (width:int)
               (height:int)
               (depth:int)
               (image:(int*int*int) list list)
               (threshold:int) = 
    match image with 
    | hd::[] -> []
    | hd::tl -> (traverse hd (List.head tl) threshold)::(EdgeDetect width height depth tl threshold)

  //
  // stripImage helper function
  //
  let rec stripImage image =
    match image with
    | [] -> []
    | (head::tail) :: tl   -> tail :: (stripImage tl)

  //
  // createRow helper function
  //
  let rec createRow image =
    match image with 
    | [] -> []
    | hd::tl -> (List.head hd) :: (createRow tl)

  //
  // traverseHor helper function
  //

  let rec traverseHor firstList strippedImage =
    match firstList with  
    | [] -> []
    | hd::tl -> (createRow strippedImage) :: (traverseHor tl (stripImage strippedImage))

  //
  // RotateRight90:
  //
  // Rotates the image to the right 90 degrees.
  //
  // Returns: updated image.
  //
  let rec RotateRight90 (width:int)
                        (height:int)
                        (depth:int)
                        (image:(int*int*int) list list) = 
    let firstList = List.head image
    let reversedImage = List.rev image
    traverseHor firstList reversedImage

