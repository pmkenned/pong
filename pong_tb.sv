`define CLK_PERIOD 20

module pong_tb;

	logic clk_50M;

	// general I/O
	logic [17:0] sw;
	logic [3:0] btns_b;
	logic [17:0] ledr;
	logic [8:0] ledg;

	// VGA
    logic HS, VS;
    logic [23:0] VGA_RGB;
    logic VGA_clk;
    logic VGA_blank;

	pong p(.*);

	logic rst;
	assign sw[17] = rst;

	initial begin
		clk_50M <= 1'b0;
		rst <= 1'b0;
		#1 rst <= 1'b1;
		#1 rst <= 1'b0;
		forever #(`CLK_PERIOD/2) clk_50M = ~clk_50M;
	end

	initial begin
		btns_b[3:0] <= 4'b1111;
		repeat(400) @(posedge clk_50M);
		$finish;
	end


endmodule: pong_tb
