import { arc } from "d3";
import React, { type FC, useState } from "react";
import type { PlanetaryComp } from "@/components/data-vis/planetary-comparison/planetaryComparison.tsx";
import { colorCoding, colorCodingSummary } from "@/components/data-vis/planetary-comparison/shared.tsx";


interface OneBarProps {
  one: PlanetaryComp;
  lancet: PlanetaryComp;
  xScale: any;
  yScale: any;
  innerRadius: number;
  energyFilter: string;
  setTooltipDataCountry(tooltipDataCountry: any): void;
}

export const OneBar: FC<OneBarProps> = ({
  one,
  lancet,
  xScale,
  yScale,
  innerRadius,
  energyFilter,
  setTooltipDataCountry
}) => {
  const [hoverState, setHoverState] = useState<boolean>(false);
  const arcPathGenerator = arc();
  const ANGLE_OFFSET = (5 * Math.PI) / 180;
  const start = (xScale(one.food) ?? 0) + ANGLE_OFFSET;
  const startLancet = (xScale(lancet.food) ?? 0) + ANGLE_OFFSET;
  const end = start + xScale.bandwidth();
  const gram = "gram";
  const calorie = "calorie";
  const path = arcPathGenerator({
    innerRadius,
    outerRadius: energyFilter === gram ? yScale(one.grams) : yScale(one.calories),
    startAngle: start,
    endAngle: end
  });
  const pathLancet = arcPathGenerator({
    innerRadius,
    outerRadius: energyFilter === gram ? yScale(lancet.grams) : yScale(lancet.calories),
    startAngle: startLancet,
    endAngle: end
  });

  const barAngle = xScale(one.food)! + xScale.bandwidth() / 2 + ANGLE_OFFSET;
  const turnLabelUpsideDown = (barAngle + Math.PI) % (2 * Math.PI) < Math.PI;
  const labelRotation = (barAngle * 180) / Math.PI - 90;
  const labelXTranslation = energyFilter ? yScale(one.grams) + 5 : yScale(one.calories) + 5;
  const labelTransform = "rotate(" + labelRotation + ")" + ",translate(" + labelXTranslation + ",0)";

  const color = colorCoding(one.type);
  const colorSummary = colorCodingSummary(one.type);

  function handleTooltipHover() {
    setHoverState(true);
    setTooltipDataCountry({
      x: 0,
      y: 0,
      datapoint: one,
      lancet: lancet
    });
  }

  function handleMouseLeave() {
    setHoverState(false);
    setTooltipDataCountry(null);
  }

  return (
    <g>
      <path
        d={path ?? undefined}
        opacity={1}
        stroke={hoverState ? "#FFC466" : "transparent"}
        strokeWidth={3}
        fill={colorSummary}
        fillOpacity={1}
        onMouseEnter={handleTooltipHover}
        onMouseLeave={handleMouseLeave}
      />
      <path
        d={pathLancet ?? undefined}
        opacity={1}
        fill={"#faf7ed"}
        fillOpacity={0.5}
        stroke={"rgba(0,0,0,0.8)"}
        strokeWidth={1.2}
        rx={1}
        onMouseEnter={handleTooltipHover}
        onMouseLeave={handleMouseLeave}
      ></path>
    </g>
  );
};
