import spatial.dsl._

@spatial object ProjectCNNColorization extends SpatialApp {

  // Define the maximum sizes for the on chip memory elements
  val WH_MAX = 226.to[Int] // 226 instead of 224 to account for 1 padding
  val DEPTH_MAX = 512.to[Int]
  val NUM_FILTERS_MAX = 512.to[Int]
  val KERNEL_SIZE_MAX = 7.to[Int] // Accounts for a 3x3 kernel that is 2 dilated

  // Define the parallelization factor for the line buffer loading
  val LB_PAR = 8.to[Int]

  // Set the size of each weight value, can alter this for more precision
  type T = FixPt[TRUE, _5, _11]

  def main(args: Array[String]): Unit = {
    /*
     * Naming conventions for convolution block parameters are as follows:
     *
     *    <parameter>_<conv block #>_<layer #>
     *
     * Where the layer # is optional if the multiple layers in the convolution block
     * share the same sizes for that particular parameter. For example, if convolution
     * block 1 has two layers, 1_1 and 1_2, but the padding is the same for both layers,
     * then the padding can be defined as pad_1 in order to reduce redundant code.
     */

    // **********************************
    //            Conv 1
    // **********************************

    // Load the weights, biases, and norm factors for the convolution block
    val w1_1_csv = loadCSV2D[T]("bw_conv1_1_weights.csv", ",")
    val b1_1_csv = loadCSV1D[T]("bw_conv1_1_bias.csv", ",")
    val w1_2_csv = loadCSV2D[T]("conv1_2_weights.csv", ",")
    val b1_2_csv = loadCSV1D[T]("conv1_2_bias.csv", ",")
    val norm_1 = loadCSV1D[T]("conv1_2norm_mean_variance.csv", ",")
    val test_image = loadCSV2D[T]("test_image.csv", ",")

    // Define the parameters of the convolution block
    val raw_kernel_size_1 = 3.to[Int]
    val raw_input_size_1 = 224.to[Int]
    val num_filters_1 = 64.to[Int]
    val depth_1_1 = 1.to[Int]
    val depth_1_2 = 64.to[Int]
    val pad_1 = 1.to[Int]
    val stride_1_1 = 1.to[Int]
    val stride_1_2 = 2.to[Int]
    val dilation_1 = 1.to[Int]

    // Calculate the new width/height given the padding and the input size
    val input_size_1 = raw_input_size_1 + (pad_1 * 2.to[Int])

    // Calculate the new kernel size given the dilation
    val kernel_size_1 = (dilation_1 * raw_kernel_size_1) + dilation_1 - 1.to[Int]

    // Initialize weights and biases in DRAM for convolution block 1
    val w1_1_2d = DRAM[T](num_filters_1, 1, raw_kernel_size_1, raw_kernel_size_1)
    val b1_1 = DRAM[T](num_filters_1)
    setMem(w1_1_2d, w1_1_csv)
    setMem(b1_1, b1_1_csv)

    val w1_2_2d = DRAM[T](num_filters_1, depth_1_2, raw_kernel_size_1, raw_kernel_size_1)
    val b1_2 = DRAM[T](num_filters_1)
    setMem(w1_2_2d, w1_2_csv)
    setMem(b1_2, b1_2_csv)

    // **********************************
    //            Conv 2
    // **********************************
    val w2_1_csv = loadCSV2D[T]("conv2_1_weights.csv", ",")
    val b2_1_csv = loadCSV1D[T]("conv2_1_bias.csv", ",")
    val w2_2_csv = loadCSV2D[T]("conv2_2_weights.csv", ",")
    val b2_2_csv = loadCSV1D[T]("conv2_2_bias.csv", ",")
    val norm_2 = loadCSV1D[T]("conv2_2norm_mean_variance.csv", ",")

    /*
    val w3_1_data = loadCSV2D[T]("XXXXX.csv", ",")
    val b3_1_data = loadCSV1D[T]("XXXXX.csv", ",")
    val w3_2_data = loadCSV2D[T]("XXXXX.csv", ",")
    val b3_2_data = loadCSV1D[T]("XXXXX.csv", ",")
    val w3_3_data = loadCSV2D[T]("XXXXX.csv", ",")
    val b3_3_data = loadCSV1D[T]("XXXXX.csv", ",")
    val norm_3 = loadCSV1D[T]("XXXXX.csv", ",")

    val w4_1_data = loadCSV2D[T]("XXXXX.csv", ",")
    val b4_1_data = loadCSV1D[T]("XXXXX.csv", ",")
    val w4_2_data = loadCSV2D[T]("XXXXX.csv", ",")
    val b4_2_data = loadCSV1D[T]("XXXXX.csv", ",")
    val w4_3_data = loadCSV2D[T]("XXXXX.csv", ",")
    val b4_3_data = loadCSV1D[T]("XXXXX.csv", ",")
    val norm_4 = loadCSV1D[T]("XXXXX.csv", ",")

    val w5_1_data = loadCSV2D[T]("XXXXX.csv", ",")
    val b5_1_data = loadCSV1D[T]("XXXXX.csv", ",")
    val w5_2_data = loadCSV2D[T]("XXXXX.csv", ",")
    val b5_2_data = loadCSV1D[T]("XXXXX.csv", ",")
    val w5_3_data = loadCSV2D[T]("XXXXX.csv", ",")
    val b5_3_data = loadCSV1D[T]("XXXXX.csv", ",")
    val norm_5 = loadCSV1D[T]("XXXXX.csv", ",")

    val w6_1_data = loadCSV2D[T]("XXXXX.csv", ",")
    val b6_1_data = loadCSV1D[T]("XXXXX.csv", ",")
    val w6_2_data = loadCSV2D[T]("XXXXX.csv", ",")
    val b6_2_data = loadCSV1D[T]("XXXXX.csv", ",")
    val w6_3_data = loadCSV2D[T]("XXXXX.csv", ",")
    val b6_3_data = loadCSV1D[T]("XXXXX.csv", ",")
    val norm_6 = loadCSV1D[T]("XXXXX.csv", ",")

    val w7_1_data = loadCSV2D[T]("XXXXX.csv", ",")
    val b7_1_data = loadCSV1D[T]("XXXXX.csv", ",")
    val w7_2_data = loadCSV2D[T]("XXXXX.csv", ",")
    val b7_2_data = loadCSV1D[T]("XXXXX.csv", ",")
    val w7_3_data = loadCSV2D[T]("XXXXX.csv", ",")
    val b7_3_data = loadCSV1D[T]("XXXXX.csv", ",")
    val norm_7 = loadCSV1D[T]("XXXXX.csv", ",")

    val w8_1_data = loadCSV2D[T]("XXXXX.csv", ",")
    val b8_1_data = loadCSV1D[T]("XXXXX.csv", ",")
    val w8_2_data = loadCSV2D[T]("XXXXX.csv", ",")
    val b8_2_data = loadCSV1D[T]("XXXXX.csv", ",")
    val w8_3_data = loadCSV2D[T]("XXXXX.csv", ",")
    val b8_3_data = loadCSV1D[T]("XXXXX.csv", ",")
    val norm_8 = loadCSV1D[T]("XXXXX.csv", ",")
  */
    // Define the output of the previous convolutional layer and the input to the next
    val input = DRAM[T](NUM_FILTERS_MAX, WH_MAX, WH_MAX)
    setMem(input, test_image)
    val output = DRAM[T](NUM_FILTERS_MAX, WH_MAX, WH_MAX)


    Accel {

      def conv(input: DRAM3[T], wh: Int, depth: Int, weights_dram: DRAM4[T], num_filters: Int,
               bias: SRAM1[T], stride: Int, padding: Int, dilation: Int, kernel_size: Int,
               lb: LineBuffer[T], weights_sram: SRAM3[T], sr: RegFile2[T], output: DRAM3[T]): Unit = {
        /*
         * Fused Convolution - Bias - ReLU functionality. Accelerator first converts the 2d weight file
         * to the 4d representation for easy indexing. Then the convolution is performed.
         *
         * Params:
         *  input         Input into the convolution layer
         *  input_wh      Input width and height. Always assumes square input
         *  depth         The depth of the input and the file
         *  weights_2d    The weights for the particular layer read in from the csv file
         *  num_filters   The number of filters in the weights file
         *  bias_dram     The bias to be added on to the convolution result
         *  stride        Stride while iterating through the input
         *  padding       0 padding added around the input
         *  dilation      Kernel dilation factor
         *  kernel_size   One dimension size of the square kernel
         */
        val weights = RegFile[T](KERNEL_SIZE_MAX, KERNEL_SIZE_MAX)
        val lineOut = SRAM[T](NUM_FILTERS_MAX)

        Foreach(0 until wh by stride, 0 until wh by stride, 0 until num_filters) { (r,c,m) =>
          weights_sram load weights_dram(m, 0.to[Int]::depth, 0.to[Int]::kernel_size, 0.to[Int]::kernel_size)
          val tmp = Reduce(Reg[T])(0 until depth) { d =>
            lb load input(d, r, 0.to[Int] :: (wh - 2.to[Int] * padding) par LB_PAR)
            Pipe {
              sr.reset(c.to[Int] == 0.to[Int])
            }

            // Dilates the kernel
            Foreach(0 until kernel_size, 0 until kernel_size) { (i, j) =>
              weights(i, j) = mux(i % 2.to[Int] == 0.to[Int] || j % 2.to[Int] == 0.to[Int], weights_sram(d, i/dilation, j/dilation), 0.to[T])
            }
            // Loads a 0 into the shift register to account for padding
            Foreach(0 until kernel_size) { i => sr(i, *) <<= mux(c < padding || c > (wh - (2.to[Int] * padding)), 0.to[T], lb(i, c-padding)) }
            Reduce(0.to[T])(0 until kernel_size, 0 until kernel_size) { (i,j) =>
              sr(i, j) * weights(i, j)
            }{_+_}
          }{_+_}
          lineOut(m) = mux(r < kernel_size || c < kernel_size, 0.to[T], max(0.to[T], tmp.value + bias(m)))
          output(0.to[Int]::num_filters, r, c) store lineOut
        }
      }

      // Convert the 2d weights into 4d weights in DRAM
      //val weights_dram = convert_weights(weights_2d, num_filters, depth, kernel_size)
      val weights_sram = SRAM[T](DEPTH_MAX, KERNEL_SIZE_MAX, KERNEL_SIZE_MAX)

      // Load the biases into SRAM
      val bias = SRAM[T](NUM_FILTERS_MAX)

      // Initialize the line buffer for the convolution to sweep through
      val lb = LineBuffer[T](KERNEL_SIZE_MAX, WH_MAX)

      val sr = RegFile[T](KERNEL_SIZE_MAX, KERNEL_SIZE_MAX)


      Sequential{
        bias load b1_1
        conv(input, input_size_1, depth_1_1, w1_1_2d, num_filters_1,
          bias, stride_1_1, pad_1, dilation_1, kernel_size_1, lb, weights_sram, sr, output)
      }
    }
    printTensor3(getTensor3(output))
  }
}
