import { arc } from "d3";
import React, { type FC, useState } from "react";
import type { PlanetaryComp } from "@/components/data-vis/planetary-comparison/planetaryComparison.tsx";
import {
  colorCoding,
  colorCodingSummary,
  type TooltipData
} from "@/components/data-vis/planetary-comparison/shared.tsx";

interface OneBarProps {
  one: PlanetaryComp;
  xScale: any;
  yScale: any;
  innerRadius: number;
  energyFilter: string;
  isMobile: boolean;
  setTooltipData(tooltipData: TooltipData | null): void;
  width: number;
  height: number;
}

export const OneBarPlanetary: FC<OneBarProps> = ({
  one,
  xScale,
  yScale,
  innerRadius,
  energyFilter,
  isMobile,
  setTooltipData,
  width,
  height
}) => {
  const arcPathGenerator = arc();

  const ANGLE_OFFSET = (5 * Math.PI) / 180;
  const start = (xScale(one.food) ?? 0) + ANGLE_OFFSET;
  const end = start + xScale.bandwidth();
  const gram = "gram";
  const calorie = "calorie";
  const path = arcPathGenerator({
    innerRadius,
    outerRadius: energyFilter === gram ? yScale(one.grams) : yScale(one.calories),
    startAngle: start,
    endAngle: end
  });

  const barAngle = xScale(one.food)! + xScale.bandwidth() / 2 + ANGLE_OFFSET;
  const turnLabelUpsideDown = (barAngle + Math.PI) % (2 * Math.PI) < Math.PI;
  const labelRotation = (barAngle * 180) / Math.PI - 90;
  const labelXTranslation = energyFilter === gram ? yScale(one.grams) + 5 : yScale(one.calories) + 5;
  const labelTransform = "rotate(" + labelRotation + ")" + ",translate(" + labelXTranslation + ",0)";
  const color = colorCoding(one.type);

  return (
    <g>
      <path
        d={path ?? undefined}
        fill={color}
        stroke={"rgba(0,0,0,0.8)"}
        fillOpacity={1}
        strokeWidth={1.2}
        className={"hover:stroke-[#dbdbd9]"}
        // strokeDasharray="6 2"

        onMouseEnter={() => {
          setTooltipData({
            x: 100,
            y: 100,
            datapoint: one
          });
        }}
        onMouseLeave={() => setTooltipData(null)}
      />
      <g transform={labelTransform} className={"font-bodyBold"}>
        <text
          textAnchor={turnLabelUpsideDown ? "end" : "start"}
          alignmentBaseline="middle"
          fill={"rgba(0,0,0,0.8)"}
          // fontWeight={"bold"}
          fontSize={isMobile ? 10 : 12}
          transform={turnLabelUpsideDown ? "rotate(180)" : "rotate(0)"}
          onMouseEnter={() => {
            setTooltipData({
              x: 100,
              y: 100,
              datapoint: one
            });
          }}
          onMouseLeave={() => setTooltipData(null)}
        >
          {one.food}
        </text>
      </g>
    </g>
  );
};
