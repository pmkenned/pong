`default_nettype none

//`define SYNTH

`define VGA_NUM_ROWS        10'd480
`define VGA_NUM_COLS        10'd640

// following in terms of 25 MHz clock
`define VGA_HS_TDISP        `VGA_NUM_COLS
`define VGA_HS_TPW          10'd96
`define VGA_HS_TFP          10'd16
`define VGA_HS_TBP          10'd48
`define VGA_HS_OFFSET      (`VGA_HS_TPW + `VGA_HS_TBP)
`define VGA_HS_TS           (`VGA_HS_OFFSET+`VGA_HS_TDISP+`VGA_HS_TFP)

// following in terms of lines
`define VGA_VS_TDISP        `VGA_NUM_ROWS
`define VGA_VS_TPW          10'd2
`define VGA_VS_TFP          10'd10
`define VGA_VS_TBP          10'd29
`define VGA_VS_OFFSET      (`VGA_VS_TPW + `VGA_VS_TBP)
`define VGA_VS_TS           (`VGA_VS_OFFSET+`VGA_VS_TDISP+`VGA_VS_TFP)

`define DIR_LEFT  1'b0
`define DIR_RIGHT 1'b1

`define DIR_UP    1'b0
`define DIR_DOWN  1'b1

`define LPAD_X    10'd10
`define RPAD_X    10'd629

`define PAD_HEIGHT   10'd40
`define PAD_WIDTH     4'd5
`define PAD_POS_INIT (10'd240-(`PAD_HEIGHT>>1))

`define BALL_X_INIT (`VGA_NUM_COLS >> 1)
`define BALL_Y_INIT (`VGA_NUM_ROWS >> 1)

`define BALL_WIDTH 4'd10

module pong(
	input logic clk_50M,

	// general I/O
	input logic [17:0] sw,
	input logic [3:0] btns_b,
	output logic [17:0] ledr,
	output logic [8:0] ledg,

	// VGA
    output logic HS, VS,
    output logic [23:0] VGA_RGB,
    output logic VGA_clk,
    output logic VGA_blank
);
	logic [3:0] btns;
	assign btns = ~btns_b;

	logic clk;
	assign clk = clk_50M;

	logic rst;
	assign rst = sw[17];

	// inputs

	logic lpad_up_btn, lpad_dn_btn;
	logic rpad_up_btn, rpad_dn_btn;
	logic do_stripes;
	logic put_into_play;

	assign lpad_dn_btn = btns[3];
	assign lpad_up_btn = btns[2];
	assign rpad_dn_btn = btns[1];
	assign rpad_up_btn = btns[0];
	assign put_into_play = sw[0];
	assign do_stripes = sw[16];

	// declaring state variables

	logic in_play, in_play_n;

	logic [2:0] lpad_score, lpad_score_n;
	logic [2:0] rpad_score, rpad_score_n;

	logic [9:0] ball_pos_x, ball_pos_x_n;
	logic [9:0] ball_pos_y, ball_pos_y_n;

	logic ball_dir_x, ball_dir_x_n;
	logic ball_dir_y, ball_dir_y_n;

	logic [25:0] game_period, game_period_n;
	logic [25:0] cycle_count, cycle_count_n;

	logic [9:0] lpad_pos, lpad_pos_n;
	logic [9:0] rpad_pos, rpad_pos_n;

	logic [9:0] lpad_min, lpad_max;
	logic [9:0] rpad_min, rpad_max;

	assign lpad_min = lpad_pos;
	assign lpad_max = lpad_pos+`PAD_HEIGHT;
	assign rpad_min = rpad_pos;
	assign rpad_max = rpad_pos+`PAD_HEIGHT;

	logic do_update;

	assign do_update = (cycle_count == game_period);

	// outputs

	assign ledg = 'b0;
	assign ledr[2:0] = rpad_score;
	assign ledr[5:3] = lpad_score;
	assign ledr[17:6] = 'b0;

	// VGA controller

	logic display_done; // TODO: is this useful?
	logic [9:0] vga_row;
	logic [9:0] vga_col;
	logic clk_25M;

    logic [2:0] stripes_color;

	vga v(.*);
	stripes s(.vga_color(stripes_color), .*);

	// TODO: use stripes module for testing

	// VGA output: function of vga_row, vga_col and ball and paddle positions

	logic in_lpad_ver, in_lpad_hor;
	logic in_rpad_ver, in_rpad_hor;
	logic in_ball_ver, in_ball_hor;
	logic in_lpad, in_rpad, in_ball;

	range_check #(.W(10)) lpad_ver_rc(.is_between(in_lpad_ver), .val(vga_row), .low(lpad_min), .high(lpad_max));
	range_check #(.W(10)) lpad_hor_rc(.is_between(in_lpad_hor), .val(vga_col), .low(`LPAD_X-`PAD_WIDTH), .high(`LPAD_X));

	range_check #(.W(10)) rpad_ver_rc(.is_between(in_rpad_ver), .val(vga_row), .low(rpad_min), .high(rpad_max));
	range_check #(.W(10)) rpad_hor_rc(.is_between(in_rpad_hor), .val(vga_col), .low(`RPAD_X-`PAD_WIDTH), .high(`RPAD_X));

	range_check #(.W(10)) ball_ver_rc(.is_between(in_ball_ver), .val(vga_row), .low(ball_pos_y), .high(ball_pos_y+`BALL_WIDTH));
	range_check #(.W(10)) ball_hor_rc(.is_between(in_ball_hor), .val(vga_col), .low(ball_pos_x), .high(ball_pos_x+`BALL_WIDTH));

	assign in_lpad = in_lpad_ver & in_lpad_hor;
	assign in_rpad = in_rpad_ver & in_rpad_hor;
	assign in_ball = in_ball_ver & in_ball_hor;

	always_comb begin
		VGA_RGB = 24'b0;

		if(do_stripes) begin
			VGA_RGB = {stripes_color[2], 7'b0, stripes_color[1], 7'b0, stripes_color[0], 7'b0};
		end else begin
			if(in_lpad)
				VGA_RGB = 24'hFF_FF_FF;
			if(in_rpad)
				VGA_RGB = 24'hFF_FF_FF;
			if(in_ball)
				VGA_RGB = 24'hFF_FF_FF;
		end
	end

	// registers

	ff_ar #(.W(1), .RV(1'b0)) in_play_ff(.q(in_play), .d(in_play_n), .*);

	ff_ar #(.W(26), .RV(26'd500_000)) game_period_reg(.q(game_period), .d(game_period_n), .*);
	ff_ar #(.W(26), .RV(26'd0)) cycle_count_reg(.q(cycle_count), .d(cycle_count_n), .*);

	ff_ar #(.W(3)) lpad_score_reg(.q(lpad_score), .d(lpad_score_n), .*);
	ff_ar #(.W(3)) rpad_score_reg(.q(rpad_score), .d(rpad_score_n), .*);

	ff_ar #(.W(10), .RV(`BALL_X_INIT)) ball_pos_x_reg(.q(ball_pos_x), .d(ball_pos_x_n), .*);
	ff_ar #(.W(10), .RV(`BALL_Y_INIT)) ball_pos_y_reg(.q(ball_pos_y), .d(ball_pos_y_n), .*);

	ff_ar #(.W(1), .RV(`DIR_LEFT)) ball_dir_x_ff(.q(ball_dir_x), .d(ball_dir_x_n), .*);
	ff_ar #(.W(1), .RV(`DIR_UP  )) ball_dir_y_ff(.q(ball_dir_y), .d(ball_dir_y_n), .*);

	ff_ar #(.W(10), .RV(`PAD_POS_INIT)) lpad_pos_reg(.q(lpad_pos), .d(lpad_pos_n), .*);
	ff_ar #(.W(10), .RV(`PAD_POS_INIT)) rpad_pos_reg(.q(rpad_pos), .d(rpad_pos_n), .*);

	// conditions

	logic ball_hit_lpad;
	logic ball_hit_rpad;
	logic ball_hit_pad;

	logic ball_off_top;
	logic ball_off_bottom;
	logic ball_off_left;
	logic ball_off_right;
	logic ball_off_sides;
	logic ball_off_wall;

	assign ball_hit_lpad = (ball_pos_x == `LPAD_X) & (ball_pos_y >= lpad_pos) & (ball_pos_y <= (lpad_pos+`PAD_HEIGHT));
	assign ball_hit_rpad = ((ball_pos_x + `BALL_WIDTH) == (`RPAD_X-`PAD_WIDTH)) & (ball_pos_y >= rpad_pos) & (ball_pos_y <= (rpad_pos+`PAD_HEIGHT));
	assign ball_hit_pad = ball_hit_lpad | ball_hit_rpad;

	assign ball_off_top     = (ball_pos_y == 10'd0)                ? 1'b1 : 1'b0;
	assign ball_off_bottom  = (ball_pos_y == `VGA_NUM_ROWS-10'd1)  ? 1'b1 : 1'b0;
	assign ball_off_left    = (ball_pos_x == 10'd0)               ? 1'b1 : 1'b0;
	assign ball_off_right   = (ball_pos_x == `VGA_NUM_COLS-10'd1) ? 1'b1 : 1'b0;
	assign ball_off_sides   = ball_off_left | ball_off_right;
	assign ball_off_wall    = ball_off_top | ball_off_bottom;

	// in play next state logic

	always_comb begin
		in_play_n = in_play;
		if(do_update) begin
			if(in_play & ball_off_sides)
				in_play_n = 1'b0;
			if(~in_play & put_into_play)
				in_play_n = 1'b1;
		end
	end

	// game period next state logic

	assign game_period_n = (do_update & ball_off_sides) ? game_period - (game_period >> 3) : game_period;

	// cycle count next state logic

	assign cycle_count_n = (do_update) ? 26'd0 : (cycle_count + 1'd1);

	// score next state logic

	assign lpad_score_n = (do_update & ball_off_right) ? lpad_score + 1'b1 : lpad_score;
	assign rpad_score_n = (do_update & ball_off_left) ? rpad_score + 1'b1 : rpad_score;

	// ball position next state logic

	// using ball_dir_x_n and ball_dir_y_n to prevent oscillation within paddle

	always_comb begin
		ball_pos_x_n = ball_pos_x;
		ball_pos_y_n = ball_pos_y;
		if(do_update) begin
			if(ball_off_sides) begin
				ball_pos_x_n = `BALL_X_INIT;
				ball_pos_y_n = `BALL_Y_INIT;
			end
			else begin
				if(in_play) begin
					if(ball_dir_x_n == `DIR_RIGHT)
						ball_pos_x_n = ball_pos_x+1'd1;
					if(ball_dir_x_n == `DIR_LEFT)
						ball_pos_x_n = ball_pos_x-1'd1;
					if(ball_dir_y_n == `DIR_DOWN)
						ball_pos_y_n = ball_pos_y+1'd1;
					if(ball_dir_y_n == `DIR_UP)
						ball_pos_y_n = ball_pos_y-1'd1;
				end
			end
		end
	end

	// ball direction next state logic

	assign ball_dir_x_n = (do_update & (ball_hit_pad | ball_off_sides))?  ~ball_dir_x : ball_dir_x;
	assign ball_dir_y_n = (do_update & ball_off_wall)?                    ~ball_dir_y : ball_dir_y;

	// paddle position next state logic

	// TODO: don't allow the paddle to move off the screen

	always_comb begin
		lpad_pos_n = lpad_pos;
		if(do_update & in_play) begin
			case({lpad_up_btn,lpad_dn_btn})
				2'b00: lpad_pos_n = lpad_pos;
				2'b01: lpad_pos_n = lpad_pos + 1'b1;
				2'b10: lpad_pos_n = lpad_pos - 1'b1;
				default: lpad_pos_n = lpad_pos;
			endcase
			if(lpad_pos_n <= 'd0)
				lpad_pos_n = lpad_pos;
			if(lpad_pos_n+`PAD_HEIGHT >= `VGA_NUM_ROWS)
				lpad_pos_n = lpad_pos;
		end
	end

	always_comb begin
		rpad_pos_n = rpad_pos;
		if(do_update & in_play) begin
			case({rpad_up_btn,rpad_dn_btn})
				2'b00: rpad_pos_n = rpad_pos;
				2'b01: rpad_pos_n = rpad_pos + 1'b1;
				2'b10: rpad_pos_n = rpad_pos - 1'b1;
				default: rpad_pos_n = rpad_pos;
			endcase
			if(rpad_pos_n <= 'd0)
				rpad_pos_n = rpad_pos;
			if(rpad_pos_n+`PAD_HEIGHT >= `VGA_NUM_ROWS)
				rpad_pos_n = rpad_pos;
		end
	end

endmodule: pong

module ff_ar #(parameter W=1, RV={W{1'b0}}) (
	input logic [W-1:0] d,
	output logic [W-1:0] q,
	input logic clk, rst);

	always_ff @(posedge clk, posedge rst) begin
		if(rst)
			q <= RV;
		else
			q <= d;
	end

endmodule: ff_ar

module vga(
    output logic display_done,
    output logic HS, VS,
    output logic VGA_clk,
    output logic VGA_blank,
    output logic [9:0] vga_row,
    output logic [9:0] vga_col,
    output logic clk_25M,
    input logic clk_50M, rst);

    logic clr_clk, clr_line, inc_line;
    logic HS_b, VS_b;
    logic [9:0] clk_cnt;
    logic [9:0] line_cnt;
    logic max_line;

    logic clr_line_50M;
    assign display_done = clr_line && ~clr_line_50M;

    always @(posedge clk_50M, posedge rst) begin
        if(rst)    clr_line_50M <= 1'b0;
        else        clr_line_50M <= clr_line;
    end

    assign HS = ~HS_b;
    assign VS = ~VS_b;

    assign clr_clk = inc_line;
    assign clr_line = clr_clk && max_line;

    always_ff @(posedge clk_50M, posedge rst) begin
        if(rst)    clk_25M <= 1'b0;
        else        clk_25M <= ~clk_25M;
    end

    logic HS_porch_b, VS_porch_b;
    range_check #(10) VS_portch_rc(.is_between(VS_porch_b), .val(vga_row), .low(10'd0), .high(`VGA_NUM_ROWS-1'b1));
    range_check #(10) HS_portch_rc(.is_between(HS_porch_b), .val(vga_col), .low(10'd0), .high(`VGA_NUM_COLS-1'b1));

    assign VGA_blank = ~(~HS_porch_b | ~VS_porch_b); // asserted low
    assign VGA_clk = ~clk_50M; // asserted low

    counter #(10,10'd0) clk_counter(.cnt(clk_cnt), .clk(clk_25M), .rst, .inc(1'b1), .clr(clr_clk));
    counter #(10,10'd0) line_counter(.cnt(line_cnt), .clk(clk_25M), .rst, .inc(inc_line), .clr(clr_line));

    compare #(10) cmp_clk(.eq(inc_line), .aGT(), .bGT(), .a(clk_cnt), .b(`VGA_HS_TS-1'b1));
    compare #(10) cmp_line(.eq(max_line), .aGT(), .bGT(), .a(line_cnt), .b(`VGA_VS_TS-1'b1));

    range_check #(10) HS_rc(.is_between(HS_b), .val(clk_cnt), .low(10'd0), .high(`VGA_HS_TPW-1'b1));
    addSub #(10) col_offset(.result(vga_col), .z(), .n(), .a(clk_cnt), .b(`VGA_HS_OFFSET), .add(1'b0));

    range_check #(10) VS_rc(.is_between(VS_b), .val(line_cnt), .low(10'd0), .high(`VGA_VS_TPW-1'b1));
    addSub #(10) row_offset(.result(vga_row), .z(), .n(), .a(line_cnt), .b(`VGA_VS_OFFSET), .add(1'b0));

endmodule: vga

module stripes(
    output logic [2:0] vga_color,
    input logic [9:0] vga_row,
    input logic [9:0] vga_col);

    logic black, blue, green, cyan, red, purple, yellow, white;

    // TODO: define stripe widths in terms of defines. Not a priority.
    range_check #(10) black_rc    (black, vga_col,10'd0,  10'd79);
    range_check #(10) blue_rc     (blue,  vga_col,10'd80, 10'd159);
    range_check #(10) green_rc    (green, vga_col,10'd160,10'd239);
    range_check #(10) cyan_rc     (cyan,  vga_col,10'd240,10'd319);
    range_check #(10) red_rc      (red,   vga_col,10'd320,10'd399);
    range_check #(10) purple_rc   (purple,vga_col,10'd400,10'd479);
    range_check #(10) yellow_rc   (yellow,vga_col,10'd480,10'd559);
    range_check #(10) white_rc    (white, vga_col,10'd560,10'd639);

    assign vga_color[2] = red | purple | yellow | white;
    assign vga_color[1] = green | cyan | yellow | white;
    assign vga_color[0] = blue | cyan | purple | white;

endmodule

module compare
    #(parameter W=1) (
    output logic eq, aGT, bGT,
    input logic [W-1:0] a, b);

    assign eq = (a == b) ? 1'b1 : 1'b0;
    assign aGT = (a > b) ? 1'b1 : 1'b0;
    assign bGT = (b > a) ? 1'b1 : 1'b0;
endmodule: compare

module addSub
    #(parameter W=1) (
    output logic [W-1:0] result,
    output logic z, n,
    input logic [W-1:0] a, b,
    input logic add);

    assign result = (add) ? (a+b) : (a-b);
    assign z = (result == 0) ? 1'b1 : 1'b0;
    assign n = result[W-1];

endmodule: addSub

module range_check
    #(parameter W=1) (
    output logic is_between,
    input logic [W-1:0] val, low, high);

    assign is_between = ((val >= low) && (val <= high)) ? 1'b1 : 1'b0;

endmodule: range_check

module counter #(parameter W=8, RV={W{1'b0}}) (
    output logic [W-1:0] cnt,
    input logic clr, inc,
    input logic clk, rst);

    logic [W-1:0] count_d;
    logic en;
    assign count_d = (clr) ? RV : (cnt+1'b1);
    assign en = inc || clr;
    ff_ar_en #(W, RV) count(.q(cnt), .d(count_d), .en, .clk, .rst);

endmodule: counter

module ff_ar_en #(parameter W=1, RV={W{1'b0}}) (
    output logic [W-1:0] q,
    input logic [W-1:0] d,
    input logic en, clk, rst);

    logic [W-1:0] mux_out;
    assign mux_out = (en) ? d : q;

    ff_ar #(W,RV) ff(.q, .d(mux_out), .clk, .rst);

endmodule: ff_ar_en

