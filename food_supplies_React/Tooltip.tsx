import React from "react";
import {
  colorCoding,
  colorCodingSummary,
  type TooltipData,
  type TooltipDataCountry
} from "@/components/data-vis/planetary-comparison/shared.tsx";

interface TooltipPros {
  width: number;
  height: number;
  tooltipData: TooltipData | null;
  isMobile: boolean;
  energyFilter: string;
}

interface TooltipCountryPros {
  width: number;
  height: number;
  tooltipDataCountry: TooltipDataCountry | null;
  isMobile: boolean;
  energyFilter: string;
}

export const Tooltip: React.FC<TooltipPros> = ({ width, height, tooltipData, isMobile, energyFilter }) => {
  // This is where we disable tooltip. When the data is null, do no draw anything
  if (!tooltipData) {
    return;
  }
  const gram = "gram";
  const colorPlanetary = colorCoding(tooltipData.datapoint.type);
  // if (!isMobile) {
  return (
    <div
      className={
        "absolute text-left text-blackSt font-body text-sm lg:text-sm p-0 overflow-visible w-auto translate-x-1/4 shadow-xl shadow-gray-500/80 pointer-events-none transition-opacity animate-fadeIn"
      }
      style={{ left: isMobile ? width * 0.1 : width * 0.01, top: isMobile ? height * 0.65 : height * 0.7 }}
    >
      <div className={" flex flex-col text-[rgba(0,0,0,0.8)] justify-between items-start"}>
        <div
          className={"p-1 flex flex-col font-headingBold text-base text-[rgba(0,0,0,0.9)] w-full"}
          style={{ background: colorPlanetary }}
        >
          {tooltipData.datapoint.food}
        </div>
        <div className={"p-1 bg-[#faf7ed] "}>
          Recommended daily intake{" "}
          <span className={"px-1 rounded-full bg-[#dbdbd9]"}>
            {energyFilter === gram
              ? `${Math.ceil(tooltipData.datapoint.grams)}g`
              : `${Math.ceil(tooltipData.datapoint.calories)}kcal`}
          </span>
        </div>
      </div>
    </div>
  );
  // }
  // return <div></div>;
};

export const TooltipCountry: React.FC<TooltipCountryPros> = ({
  width,
  height,
  tooltipDataCountry,
  isMobile,
  energyFilter
}) => {
  // This is where we disable tooltip. When the data is null, do not draw anything
  if (!tooltipDataCountry) {
    return;
  }

  // When knowing exactly where the tooltip should be regardless of the item it hovers, we can specify position easier
  const color = colorCodingSummary(tooltipDataCountry.datapoint.type);

  const gram = "gram";
  if (!isMobile) {
    return (
      <div
        className={
          "absolute text-left text-blackSt font-body text-sm lg:text-sm p-0 overflow-visible w-full shadow-xl shadow-gray-500/80 pointer-events-none transition-opacity animate-fadeIn"
        }
        style={{ left: 0, top: height * 0.85 }}
      >
        <div
          className={"px-2 flex flex-row font-headingBold text-base text-[#faf7ed] justify-between items-center"}
          style={{ background: color }}
        >
          <div className={` `}>{tooltipDataCountry.datapoint.food}</div>
          <div className={""}>
            {(() => {
              const value =
                Math.ceil((tooltipDataCountry.datapoint.grams * 100) / tooltipDataCountry.lancet.grams) - 100;

              return `${value > 0 ? "+" : ""}${value}%`;
            })()}
          </div>
        </div>
        <div className={"px-2 py-1 bg-[#faf7ed] flex flex-col"}>
          <div className={" align-text-center"}>
            Supplied{" "}
            <span className={"px-1 rounded-full bg-[#FFC466]"}>
              {energyFilter === gram
                ? `${Math.ceil(tooltipDataCountry.datapoint.grams)}g`
                : `${Math.ceil(tooltipDataCountry.datapoint.calories)}kcal`}{" "}
            </span>
          </div>
          <div>
            vs.planetary target{" "}
            <span className={"px-1 rounded-full bg-[#dbdbd9]"}>
              {energyFilter === gram
                ? `${Math.ceil(tooltipDataCountry.lancet.grams)}g`
                : `${Math.ceil(tooltipDataCountry.lancet.calories)}kcal`}{" "}
            </span>
          </div>
        </div>
      </div>
    );
  }
  return (
    <div
      className={
        "absolute text-left text-blackSt font-body text-sm lg:text-sm p-0 overflow-visible w-[65%] shadow-xl shadow-gray-500/80 pointer-events-none transition-opacity animate-fadeIn"
      }
      style={{ left: width * 0.17, top: height * 0.85 }}
    >
      <div
        className={"px-2 flex flex-row font-headingBold text-base text-[#faf7ed] justify-between items-center"}
        style={{ background: color }}
      >
        <div className={` `}>{tooltipDataCountry.datapoint.food}</div>
        <div className={""}>
          {(() => {
            const value = Math.ceil((tooltipDataCountry.datapoint.grams * 100) / tooltipDataCountry.lancet.grams) - 100;

            return `${value > 0 ? "+" : ""}${value}%`;
          })()}
        </div>
      </div>
      <div className={"px-2 py-1 bg-[#faf7ed] flex flex-col"}>
        <div className={" align-text-center"}>
          Supplied{" "}
          <span className={"px-1 rounded-full bg-[#FFC466]"}>
            {energyFilter === gram
              ? `${Math.ceil(tooltipDataCountry.datapoint.grams)}g`
              : `${Math.ceil(tooltipDataCountry.datapoint.calories)}kcal`}{" "}
          </span>
        </div>
        <div>
          vs. planetary intake target{" "}
          <span className={"px-1 rounded-full bg-[#dbdbd9]"}>
            {energyFilter === gram
              ? `${Math.ceil(tooltipDataCountry.lancet.grams)}g`
              : `${Math.ceil(tooltipDataCountry.lancet.calories)}kcal`}{" "}
          </span>
        </div>
      </div>
    </div>
  );
};
